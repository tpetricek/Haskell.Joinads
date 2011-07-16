{-# LINE 1 "Resumptions.jhs" #-}
module Main (main) where
{-# LINE 1 "Resumptions.jhs" #-}
import Control.Monad (Monad)
{-# LINE 2 "Resumptions.jhs" #-}
import Control.Monad.Trans
{-# LINE 3 "Resumptions.jhs" #-}
import Control.Monad.Joinads
 
{-# LINE 12 "Resumptions.jhs" #-}
data (Monad m) => Resumption m r = Step (m (Resumption m r))
                                 | Done r
                                 | Fail
{-# LINE 17 "Resumptions.jhs" #-}
action a
  = Step
      (do r <- a
          return $ Done r)
 
{-# LINE 19 "Resumptions.jhs" #-}
run :: (Monad m) => Resumption m r -> m r
{-# LINE 20 "Resumptions.jhs" #-}
run (Done r) = return r
{-# LINE 21 "Resumptions.jhs" #-}
run (Step m) = m >>= run
{-# LINE 22 "Resumptions.jhs" #-}
run (Fail) = fail "Computation failed"
 
{-# LINE 29 "Resumptions.jhs" #-}
instance (Monad m) => Monad (Resumption m) where
        {-# LINE 31 "Resumptions.jhs" #-}
        return a = Done a
        {-# LINE 35 "Resumptions.jhs" #-}
        (Done r) >>= f = Step $ return (f r)
        {-# LINE 36 "Resumptions.jhs" #-}
        (Step s) >>= f
          = Step
              (do next <- s
                  return $ next >>= f)
        {-# LINE 37 "Resumptions.jhs" #-}
        Fail >>= f = Fail
 
{-# LINE 39 "Resumptions.jhs" #-}
instance MonadTrans Resumption where
        {-# LINE 42 "Resumptions.jhs" #-}
        lift = action
 
{-# LINE 44 "Resumptions.jhs" #-}
instance (Monad m) => MonadZip (Resumption m) where
        {-# LINE 47 "Resumptions.jhs" #-}
        mzip (Done a) (Done b) = Done (a, b)
        {-# LINE 48 "Resumptions.jhs" #-}
        mzip (Fail) _ = Fail
        {-# LINE 49 "Resumptions.jhs" #-}
        mzip _ (Fail) = Fail
        {-# LINE 50 "Resumptions.jhs" #-}
        mzip sa sb
          = Step
              (do a <- step sa
                  b <- step sb
                  return $ mzip a b)
          where {-# LINE 51 "Resumptions.jhs" #-}
                step (Done r) = return $ Done r
                {-# LINE 52 "Resumptions.jhs" #-}
                step (Fail) = return $ Fail
                {-# LINE 53 "Resumptions.jhs" #-}
                step (Step sa) = sa
 
{-# LINE 55 "Resumptions.jhs" #-}
instance (Monad m) => MonadZero (Resumption m) where
        {-# LINE 57 "Resumptions.jhs" #-}
        mzero = Fail
 
{-# LINE 59 "Resumptions.jhs" #-}
instance (Monad m) => MonadOr (Resumption m) where
        {-# LINE 63 "Resumptions.jhs" #-}
        morelse (Fail) c = c
        {-# LINE 64 "Resumptions.jhs" #-}
        morelse c (Fail) = c
        {-# LINE 65 "Resumptions.jhs" #-}
        morelse (Done a) _ = Done a
        {-# LINE 66 "Resumptions.jhs" #-}
        morelse _ (Done a) = Done a
        {-# LINE 67 "Resumptions.jhs" #-}
        morelse (Step sa) b = Step (sa >>= return . (morelse b))
 
{-# LINE 69 "Resumptions.jhs" #-}
instance (Monad m) => MonadAlias (Resumption m) where
        {-# LINE 73 "Resumptions.jhs" #-}
        malias = return
 
{-# LINE 82 "Resumptions.jhs" #-}
printLoop :: [Char] -> Integer -> a -> Resumption IO a
{-# LINE 83 "Resumptions.jhs" #-}
printLoop str count result
  = do lift $ putStrLn str
       (if count == 1 then return result else
          printLoop str (count - 1) result)
{-# LINE 90 "Resumptions.jhs" #-}
animalsS
  = do c <- printLoop "meow" 2 "cat"
       d <- printLoop "woof" 3 "dog"
       return $ c ++ " and " ++ d
{-# LINE 96 "Resumptions.jhs" #-}
animalsP
  = (malias (printLoop "meow" 2 "cat")) >>=
      (\ avar1 ->
         (malias (printLoop "woof" 3 "dog")) >>=
           (\ avar2 ->
              (((avar1) `mzip` (avar2) >>=
                  \ jvar1 ->
                    case (jvar1) of
                        (c, d) -> return (return $ c ++ " and " ++ d)
                        _ -> mzero))
                >>= id))
{-# LINE 102 "Resumptions.jhs" #-}
main
  = do putStrLn "\nSequential composition:"
       s <- run animalsS
       putStrLn $ s
       putStrLn "\nParallel composition:"
       s <- run animalsP
       putStrLn $ s
{-# LINE 115 "Resumptions.jhs" #-}
guard b = if b then return () else mzero
 
{-# LINE 117 "Resumptions.jhs" #-}
test ::
       Resumption IO (Maybe Int) ->
         Resumption IO (Maybe Int) -> Resumption IO Bool
{-# LINE 118 "Resumptions.jhs" #-}
test c1 c2
  = (malias (c1)) >>=
      (\ avar1 ->
         (malias (c2)) >>=
           (\ avar2 ->
              (((avar1) `mzip` (avar2) >>=
                  \ jvar1 ->
                    case (jvar1) of
                        (Just a, Just b)
                          -> return
                               (do lift $ print "Both produced value"
                                   guard (a == b)
                                   return True)
                        _ -> mzero)
                 `morelse`
                 ((avar1) `mzip` (avar2) >>=
                    \ jvar2 ->
                      case (jvar2) of
                          (_, _)
                            -> return
                                 (do lift $ print "Some value missing"
                                     return False)
                          _ -> mzero))
                >>= id))
{-# LINE 130 "Resumptions.jhs" #-}
test' c1 c2
  = malias c1 >>=
      \ a1 ->
        malias c2 >>=
          \ a2 ->
            ((a1 `mzip` a2 >>=
                \ t ->
                  case t of
                      (Just a, Just b)
                        -> do lift $ print "Both produced value"
                              guard (a == b)
                              return True
                      _ -> mzero)
               `morelse`
               (a1 `mzip` a2 >>=
                  \ _ ->
                    do lift $ print "Some value missing"
                       return False))
{-# LINE 145 "Resumptions.jhs" #-}
test'' c1 c2
  = malias c1 >>=
      \ a1 ->
        malias c2 >>=
          \ a2 ->
            ((a1 `mzip` a2 >>=
                \ t ->
                  case t of
                      (Just a, Just b)
                        -> return
                             (do lift $ print "Both produced value"
                                 guard (a == b)
                                 return True)
                      _ -> mzero)
               `morelse`
               (a1 `mzip` a2 >>=
                  \ _ ->
                    return
                      (do lift $ print "Some value missing"
                          return False)))
              >>= id
