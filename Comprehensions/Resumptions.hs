{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MonadComprehensions #-}

import Control.Monad
import Control.Monad.Zip
import Control.Monad.Trans

-------------------------------------------------------------------------------
-- Poor man's concurrency using Resumptions (not continuations)
--  'zip'  implements parallel composition
--  'bind' implements sequential composition
-------------------------------------------------------------------------------

data Monad m => Resumption m r 
  = Step (m (Resumption m r))
  | Done r

action a = Step [ Done r | r <- a ]

run :: Monad m => Resumption m r -> m r
run (Done r) = return r
run (Step m) = m >>= run

action' a = Step $ do 
  r <- a
  return (Done r)

-------------------------------------------------------------------------------

instance Monad m => Monad (Resumption m) where
  return a = Done a
  (Done r) >>= f = Step $ return (f r)
  (Step s) >>= f = Step [ next >>= f | next <- s ]

instance MonadTrans Resumption where
  lift = action


printLoop :: [Char] -> Integer -> a -> Resumption IO a
printLoop str count result = do
  lift $ putStrLn str
  ( if count == 1 then return result					-- parens needed by preprocessor, but not GHC
    else printLoop str (count - 1) result )

main' = run $ printLoop "meow" 3 "cat"

-------------------------------------------------------------------------------

instance Monad m => MonadZip (Resumption m) where
  mzip (Done a) (Done b) = Done (a, b)
  mzip sa sb = Step [ mzip a b | a <- step sa, b <- step sb ]
    where step (Done r) = return $ Done r 
          step (Step sa) = sa

animalsS = 
  [ c ++ " and " ++ d 
      | c <- printLoop "meow" 2 "cat" 
      , d <- printLoop "woof" 3 "dog" ]

animalsP = 
  [ c ++ " and " ++ d 
      | c <- printLoop "meow" 2 "cat" 
      | d <- printLoop "woof" 3 "dog" ]

main :: IO ()
main = do
  s <- run animalsP
  putStrLn $ s

  s <- run animalsS
  putStrLn $ s
