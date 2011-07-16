{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MonadComprehensions #-}

import Control.Monad
import Control.Monad.Zip
import Control.Parallel
import Control.Parallel.Strategies
import Data.Time.Clock (diffUTCTime, getCurrentTime)

-------------------------------------------------------------------------------
-- Utilities: Simple functions (omitted from article)
-------------------------------------------------------------------------------

nfib :: Integer -> Integer
nfib 0 = 0
nfib 1 = 1
nfib n = nfib (n-1) + nfib (n-2)

evalWithTimer f = do
  putStrLn "starting..."
  start <- getCurrentTime
  putStrLn $ "Result: " ++ (show f)
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."

-------------------------------------------------------------------------------
-- Example: Writing parallel Fibonacci using Strategies
-------------------------------------------------------------------------------

fib38 = runEval $ do 
  a <- rpar $ nfib 36
  b <- rseq $ nfib 37
  return $ a + b

-------------------------------------------------------------------------------
-- xDefinition of Eval monad and MonadZip instance
-------------------------------------------------------------------------------

{-
-- This is defined in the 'parallel' library (no need to redefine)
data Eval a = Done a

runEval :: Eval a -> a
runEval (Done x) = x
  
instance Monad Eval where
  return x = Done x
  Done x >>= k = k x
-}

instance MonadZip Eval where
  -- To evaluate two computations in parallel, spawn the first one in 
  -- background using 'rpar' and evaluate the other sequentially using 'rseq'
  mzip ea eb = 
    [ (a, b) | a <- rpar $ runEval ea, 
               b <- rseq $ runEval eb ]

-------------------------------------------------------------------------------
-- Example: calculating Fibonacci numbers
-------------------------------------------------------------------------------

fibTask :: Integer -> Eval Integer
fibTask n = return $ nfib n


-- Run the two tasks in sequence using ',' notation
fib38seq = 
  [ a + b | a <- fibTask 36
          , b <- fibTask 37 ]

-- Run the two tasks in parallel using '|' notation
fib38par = 
   [ a + b | a <- fibTask 36
           | b <- fibTask 37 ]


-- Recursive Fibonacci function with threshold 35
pfib n | n <= 35 = return $ nfib n
pfib n = [ a + b | a <- pfib $ n - 1 | b <- pfib $ n - 2 ]


main = do
  putStrLn "Note: Run in parallel using 'parallel +RTS -N2 -RTS'"
  putStrLn "\nFib 38 - sequential version:"
  evalWithTimer $ runEval fib38seq

  putStrLn "\nFib 38 - parallel version:"
  evalWithTimer $ runEval fib38par

  putStrLn "\nRecursive fib 38 - parallel version:"
  evalWithTimer $ runEval (pfib 38)

-------------------------------------------------------------------------------
-- Zip comprehensions provide useful sanity check that we can actually
-- parallelize code. Changing the following to zip comprehension gives a
-- compile-time error, because there is a data dependency
-- (but this is not easy to see when using 'do' notation)
-------------------------------------------------------------------------------


ack :: Integer -> Integer -> Eval Integer
ack 0 n = return $ n + 1
ack m 0 = ack (m - 1) 1
ack m n = [ a | n' <- ack m (n - 1)
              , a <- ack (m - 1) n' ]
               