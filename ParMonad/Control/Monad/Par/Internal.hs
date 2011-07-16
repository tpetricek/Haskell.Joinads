{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP
	     #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- Enables various logging output that help with debugging of cancellation tokens
-- # define DEBUG_CANCELLATION

-- | This module exposes the internals of the @Par@ monad so that you
-- can build your own scheduler or other extensions.  Do not use this
-- module for purposes other than extending the @Par@ monad with new
-- functionality.

module Control.Monad.Par.Internal (
   Trace(..), TraceStep(..), Sched(..), Par(..),
   IVar(..), IVarContents(..),
   sched,
   runPar, runPar_, runParAsync, runParAsyncHelper,
   new, newFull, newFull_, get, put_, put,
   pollIVar, yield,
   newBlocking, 

   -- Cancellation
   CancelToken, newCancelToken, wrapCancel,
   getCancelToken, setCancelToken, cancel
 ) where


import System.Random
import Control.Monad as M hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent hiding (yield)
import GHC.Conc hiding (yield)
import Control.DeepSeq
import Control.Applicative
-- import Text.Printf

-- ---------------------------------------------------------------------------
-- A trace is a trace step (optionally) associated with a cancellation
-- token. When the token is cancelled and a trace is picked from a queue
-- it will be dropped (without performing/evaluating it)

data Trace = Trace (Maybe CancelToken) TraceStep

data TraceStep 
  = forall a . Get (IVar a) (Maybe CancelToken -> a -> Trace)
  | forall a . Put (IVar a) a Trace
  -- Boolean specifies whether the variable to be created is blocking
  -- (True means it will block when a second write attempt is made, 
  --  instead of raising an exception)
  | forall a . New Bool (IVarContents a) (Maybe CancelToken -> IVar a -> Trace)
  | forall a . Bind Trace
  | Fork Trace Trace
  | Done
  | Yield Trace
  -- Support for the cancellation of computations
  | Cancel CancelToken Trace
  | NewCtoken (CancelToken -> Trace)


-- For debugging purposes (printing information about trace steps)
instance Show TraceStep where
  show (Get _ _) = "Get"
  show (Put _ _ _) = "Put"
  show (New _ _ _) = "New"
  show (Bind _) = "Bind"
  show (Fork _ _) = "Fork"
  show Done = "Done"
  show (Yield _) = "Yield"
  show (Cancel _ _) = "Cancel"
  show (NewCtoken _) = "NewCtoken"

-- ---------------------------------------------------------------------------

-- | The main scheduler loop.
sched :: Bool -> Sched -> Trace -> IO ()
sched _doSync queue trace = loop trace
 where 
  loop (Trace tok step) = do
    -- Check for cancellation - if the token is set & cancelled
    -- we throw away the current 'step' (and replace it with 'Done' trace
    -- to schedule some other work), otherwise we continue as normal
    cancelled <- case tok of 
                   Nothing -> return False
                   Just b -> readIORef $ getTokenRef b
    
#ifdef DEBUG_CANCELLATION
    let dtok = fmap (\(id, _) -> (id, cancelled)) tok
    putStrLn $ "Sched (token = " ++ (show dtok) ++ "): " ++ (show step)
#endif
    step' <- if cancelled then (do 
#ifdef DEBUG_CANCELLATION
               -- Print information about trace cancellation
               putStrLn $ "Cancelling at " ++ (show step)
#endif
               return Done)
             else (return step)
    -- Continue processing trace (after cancellation check is done)       
    loop2 step' tok

  loop2 t tok = case t of
    NewCtoken f -> do
      tok <- createNewToken
      loop $ f tok

    Cancel token t -> do 
      writeIORef (getTokenRef token) True

#ifdef DEBUG_CANCELLATION
      -- Print information about current work queues
      let (tokid, _) = token
      putStrLn $ "\n****** Cancelled: " ++ (show tokid) ++ " ******"
      let Sched { scheds } = queue
      mapM_ (\ Sched { workpool } -> do
        trace <- readIORef workpool
        putStrLn "------------"
        mapM_ (\ (Trace tok st) -> do
          tval <- case tok of 
                    Nothing -> return Nothing
                    Just (tokid, t) -> readIORef t >>= \tv -> return $ Just (tokid, tv)
          putStrLn $ "  [" ++ (show (tokid, tval)) ++ "]: " ++ (show st)) trace) scheds
      putStrLn ""
#endif

      loop t

    Bind t -> do
        pushWork queue t
        loop (Trace Nothing Done)

    New blocking a f -> do
      r <- newIORef a
      loop (f tok (IVar blocking r))

    Get (IVar _ v) c -> do
      e <- readIORef v
      case e of
         Full a -> loop (c tok a)
         _other -> do
           r <- atomicModifyIORef v $ \e -> case e of
                        Empty    -> (Blocked [c tok], reschedule queue)
                        Full a   -> (Full a,      loop (c tok a))
                        Blocked cs -> (Blocked ((c tok):cs), reschedule queue)
           r

    Put (IVar blocking v) a t  -> do
      -- When the variable is blocking, the value of 'cs' may be
      -- Nothing which means that the caller should be blocked
      -- (Just contains blocked traces to be resumed)
      cs <- atomicModifyIORef v $ \e -> case (e, blocking) of
               (Empty, _)        -> (Full a, Just [])
               (Full _, False)   -> error "multiple put"
               (Full a, True)    -> (Full a, Nothing)
               (Blocked cs, _)   -> (Full a, Just cs)

      case cs of
        Just cs -> -- Value set - unblock waiting traces
                   do mapM_ (pushWork queue. ($a)) cs
                      loop t
        Nothing -> -- Silently block the trace 
                   do loop (Trace Nothing Done)

    Fork child parent -> do
         pushWork queue child
         loop parent

    Done ->
         if _doSync
	 then reschedule queue
-- We could fork an extra thread here to keep numCapabilities workers
-- even when the main thread returns to the runPar caller...
         else do putStrLn " [par] Forking replacement thread..\n"
                 forkIO (reschedule queue); return ()
-- But even if we don't we are not orphaning any work in this
-- threads work-queue because it can be stolen by other threads.
--	 else return ()

    Yield parent -> do 
        -- Go to the end of the worklist:
        let Sched { workpool } = queue
        -- TODO: Perhaps consider Data.Seq here.
	-- This would also be a chance to steal and work from opposite ends of the queue.
        atomicModifyIORef workpool $ \ts -> (ts++[parent], ())
	reschedule queue


-- | Process the next item on the work queue or, failing that, go into
--   work-stealing mode.
reschedule :: Sched -> IO ()
reschedule queue@Sched{ workpool } = do
  e <- atomicModifyIORef workpool $ \ts ->
         case ts of
           []      -> ([], Nothing)
           (t:ts') -> (ts', Just t)
  case e of
    Nothing -> steal queue
    Just t  -> sched True queue t


-- RRN: Note -- NOT doing random work stealing breaks the traditional
-- Cilk time/space bounds if one is running strictly nested (series
-- parallel) programs.

-- | Attempt to steal work or, failing that, give up and go idle.
steal :: Sched -> IO ()
steal q@Sched{ idle, scheds, no=my_no } = do
  -- printf "cpu %d stealing\n" my_no
  go scheds
  where
    go [] = do m <- newEmptyMVar
               r <- atomicModifyIORef idle $ \is -> (m:is, is)
               if length r == numCapabilities - 1
                  then do
                     -- printf "cpu %d initiating shutdown\n" my_no
                     mapM_ (\m -> putMVar m True) r
                  else do
                    done <- takeMVar m
                    if done
                       then do
                         -- printf "cpu %d shutting down\n" my_no
                         return ()
                       else do
                         -- printf "cpu %d woken up\n" my_no
                         go scheds
    go (x:xs)
      | no x == my_no = go xs
      | otherwise     = do
         r <- atomicModifyIORef (workpool x) $ \ ts ->
                 case ts of
                    []     -> ([], Nothing)
                    (x:xs) -> (xs, Just x)
         case r of
           Just t  -> do
              -- printf "cpu %d got work from cpu %d\n" my_no (no x)
              sched True q t
           Nothing -> go xs

-- | If any worker is idle, wake one up and give it work to do.
pushWork :: Sched -> Trace -> IO ()
pushWork Sched { workpool, idle } t = do
  atomicModifyIORef workpool $ \ts -> (t:ts, ())
  idles <- readIORef idle
  when (not (null idles)) $ do
    r <- atomicModifyIORef idle (\is -> case is of
                                          [] -> ([], return ())
                                          (i:is) -> (is, putMVar i False))
    r -- wake one up

data Sched = Sched
    { no       :: {-# UNPACK #-} !Int,
      workpool :: IORef [Trace],
      idle     :: IORef [MVar Bool],
      scheds   :: [Sched] -- Global list of all per-thread workers.
    }
--  deriving Show

-- Represents a token that can be created and later used to cancel a computation.
-- (When debugging, the token also has some ID generated randomly for tracking)
type CancelToken = (Int, IORef Bool)

getTokenRef :: CancelToken -> IORef Bool
getTokenRef (_, r) = r

createNewToken :: IO CancelToken
createNewToken = do 
  tok <- newIORef False
  rnd <- randomRIO (1000, 9999)
  return (rnd, tok)

-- Cancellation token is kept through the evaluation. It can be changed by some
-- operations (such as 'setCancelToken' below). When given a cancellation token
-- and a continuation, the 'Par' does something and eventually calls the continuation
-- with the result and a new cancellation token (to be used for marking created traces)
newtype Par a = Par {
    runCont :: Maybe CancelToken -> (Maybe CancelToken -> a -> Trace) -> Trace
}

instance Functor Par where
    fmap f m = Par $ \tok c -> runCont m tok (\t v -> c t (f v))

instance Monad Par where
    return a = Par $ \tok c -> c tok a
    m >>= k  = Par $ \tok c -> runCont m tok $ \t a -> runCont (k a) t c

instance Applicative Par where
   (<*>) = ap
   pure  = return

-- IVar can be either blocking or default (failing)
-- This determines the behvaior when putting a value into 
-- a variable that is already full (i.e. error vs. block)
data IVar a = IVar Bool (IORef (IVarContents a))

-- Forcing evaluation of a IVar is fruitless.
instance NFData (IVar a) where
  rnf _ = ()


-- From outside the Par computation we can peek.  But this is nondeterministic.
pollIVar :: IVar a -> IO (Maybe a)
pollIVar (IVar _ ref) = 
  do contents <- readIORef ref
     case contents of 
       Full x -> return (Just x)
       _      -> return (Nothing)

-- Note: Blocked computations do not expect cancellation token as an argument
-- because they carry their own token (they are different 'threads' than the
-- one that resumes them)
data IVarContents a = Full a | Empty | Blocked [a -> Trace]

wrapCancel m = do
  optTok <- getCancelToken
  optCanc <- case optTok of
    Nothing -> do
      tok <- newCancelToken
      setCancelToken $ Just tok
      return $ Just tok
    Just _ -> 
      return Nothing

  res <- m

  case optCanc of
    Nothing -> return ()
    Just tok -> do
      setCancelToken optTok
      cancel tok      
  return res


{-# INLINE runPar_internal #-}
runPar_internal :: Bool -> Par a -> a
runPar_internal _doSync x = unsafePerformIO $ do
   workpools <- replicateM numCapabilities $ newIORef []
   idle <- newIORef []
   let states = [ Sched { no=x, workpool=wp, idle, scheds=states }
                | (x,wp) <- zip [0..] workpools ]

#if __GLASGOW_HASKELL__ >= 701 /* 20110301 */
    --
    -- We create a thread on each CPU with forkOnIO.  The CPU on which
    -- the current thread is running will host the main thread; the
    -- other CPUs will host worker threads.
    --
    -- Note: GHC 7.1.20110301 is required for this to work, because that
    -- is when threadCapability was added.
    --
   (main_cpu, _) <- threadCapability =<< myThreadId
#else
    --
    -- Lacking threadCapability, we always pick CPU #0 to run the main
    -- thread.  If the current thread is not running on CPU #0, this
    -- will require some data to be shipped over the memory bus, and
    -- hence will be slightly slower than the version above.
    --
   let main_cpu = 0
#endif

   m <- newEmptyMVar
   forM_ (zip [0..] states) $ \(cpu,state) ->
        forkOnIO cpu $
          if (cpu /= main_cpu)
             then reschedule state
             else do
                  rref <- newIORef Empty
                  sched _doSync state $ runCont (x >>= put_ (IVar False rref)) Nothing (\tok _ -> Trace tok Done)
                  readIORef rref >>= putMVar m

   r <- takeMVar m
   case r of
     Full a -> return a
     _ -> error "no result"


runPar :: Par a -> a
runPar = runPar_internal True

runPar_ :: Par a -> a
runPar_ = runPar_internal True . wrapCancel

-- | An asynchronous version in which the main thread of control in a
-- Par computation can return while forked computations still run in
-- the background.  
runParAsync :: Par a -> a
runParAsync = runPar_internal False

-- | An alternative version in which the consumer of the result has
-- | the option to "help" run the Par computation if results it is
-- | interested in are not ready yet.
runParAsyncHelper :: Par a -> (a, IO ())
runParAsyncHelper = undefined -- TODO: Finish Me.

-- -----------------------------------------------------------------------------

-- | creates a new @IVar@. the variable is failing which means that 
-- attempt to put a value into a full variable causes error.
new :: Par (IVar a)
new  = Par $ \tok c -> Trace tok (New False Empty c)

-- | creates a new @IVar@ that contains a value
newFull :: NFData a => a -> Par (IVar a)
newFull x = deepseq x (Par $ \tok c -> Trace tok (New False (Full x) c))

-- | creates a new @IVar@ that contains a value (head-strict only)
newFull_ :: a -> Par (IVar a)
newFull_ !x = Par $ \tok c -> Trace tok (New False (Full x) c)

-- | creates a new @IVar@ that is blocking. this means that 
-- attempt to put a value into a full variable blocks the caller 
newBlocking :: Par (IVar a)
newBlocking  = Par $ \tok c -> Trace tok (New True Empty c)

-- | read the value in a @IVar@.  The 'get' can only return when the
-- value has been written by a prior or parallel @put@ to the same
-- @IVar@.
get :: IVar a -> Par a
get v = Par $ \tok c -> Trace tok (Get v c)

-- | like 'put', but only head-strict rather than fully-strict.
put_ :: IVar a -> a -> Par ()
put_ v !a = Par $ \tok c -> Trace tok (Put v a (c tok ()))

-- | put a value into a @IVar@.  Multiple 'put's to the same @IVar@
-- are not allowed, and result in a runtime error.
--
-- 'put' fully evaluates its argument, which therefore must be an
-- instance of 'NFData'.  The idea is that this forces the work to
-- happen when we expect it, rather than being passed to the consumer
-- of the @IVar@ and performed later, which often results in less
-- parallelism than expected.
--
-- Sometimes partial strictness is more appropriate: see 'put_'.
--
put :: NFData a => IVar a -> a -> Par ()
put v a = deepseq a (Par $ \tok c -> Trace tok (Put v a (c tok ())))

-- | Allows other parallel computations to progress.  (should not be
-- necessary in most cases).
yield :: Par ()
yield = Par $ \tok c -> Trace tok (Yield (c tok ()))

-- -----------------------------------------------------------------------------

-- | creates a new @CancelToken@ (that is not cancelled)
newCancelToken :: Par CancelToken
newCancelToken = Par $ \tok c -> Trace tok (NewCtoken $ c tok)

-- | sets the current @CancelToken@ of the computation
setCancelToken :: Maybe CancelToken -> Par ()
setCancelToken tok = Par $ \_ c -> c tok ()

-- | returns the current @CancelToken@ of the computation
-- (if there is a cancellation token associated with it)
getCancelToken :: Par (Maybe CancelToken)
getCancelToken = Par $ \tok c -> c tok tok

-- | cancels the specified @CancelToken@. this means that all traces
-- that were created using this token will be eventually cancelled (as they
-- reach the next step such as bind)
cancel :: CancelToken -> Par ()
cancel tok = Par $ \tinner c -> Trace tinner (Cancel tok (c tinner ()))
