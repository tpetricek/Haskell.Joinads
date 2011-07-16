{-# LINE 1 "ParMonad.jhs" #-}
module Main (main) where
{-# LINE 1 "ParMonad.jhs" #-}
import Data.Int
{-# LINE 2 "ParMonad.jhs" #-}
import System.Environment
{-# LINE 3 "ParMonad.jhs" #-}
import Control.Monad.Par
{-# LINE 4 "ParMonad.jhs" #-}
import Data.Time.Clock (diffUTCTime, getCurrentTime)
{-# LINE 5 "ParMonad.jhs" #-}
import GHC.Conc (pseq, par)
{-# LINE 7 "ParMonad.jhs" #-}
import Control.Monad.Joinads
{-# LINE 14 "ParMonad.jhs" #-}
evalWithTimer f
  = do putStrLn "starting..."
       start <- getCurrentTime
       putStrLn $ "Result: " ++ (show f)
       end <- getCurrentTime
       putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
 
{-# LINE 21 "ParMonad.jhs" #-}
type NumType = Int64
 
{-# LINE 24 "ParMonad.jhs" #-}
fib :: NumType -> NumType
{-# LINE 25 "ParMonad.jhs" #-}
fib 0 = 1
{-# LINE 26 "ParMonad.jhs" #-}
fib 1 = 1
{-# LINE 27 "ParMonad.jhs" #-}
fib x = fib (x - 2) + fib (x - 1) + 1
 
{-# LINE 30 "ParMonad.jhs" #-}
isPrime :: NumType -> Bool
{-# LINE 31 "ParMonad.jhs" #-}
isPrime 1 = True
{-# LINE 32 "ParMonad.jhs" #-}
isPrime 2 = True
{-# LINE 33 "ParMonad.jhs" #-}
isPrime n = divisible 2
  where {-# LINE 35 "ParMonad.jhs" #-}
        divisible m
          | n `rem` m == 0 = False
          | m * m > n = True
          | otherwise = divisible (m + 1)
 
{-# LINE 45 "ParMonad.jhs" #-}
parfibStrat :: NumType -> NumType
{-# LINE 46 "ParMonad.jhs" #-}
parfibStrat n | n < 25 = fib n
{-# LINE 47 "ParMonad.jhs" #-}
parfibStrat n = x `par` y `pseq` (x + y)
  where {-# LINE 49 "ParMonad.jhs" #-}
        x = parfibStrat (n - 1)
        {-# LINE 50 "ParMonad.jhs" #-}
        y = parfibStrat (n - 2)
 
{-# LINE 53 "ParMonad.jhs" #-}
parfibMon :: NumType -> Par NumType
{-# LINE 54 "ParMonad.jhs" #-}
parfibMon n | n < 25 = return $ fib n
{-# LINE 55 "ParMonad.jhs" #-}
parfibMon n
  = do xf <- spawn_ $ parfibMon (n - 1)
       y <- parfibMon (n - 2)
       x <- get xf
       return (x + y)
{-# LINE 63 "ParMonad.jhs" #-}
mainFib
  = do let {-# LINE 65 "ParMonad.jhs" #-}
           size = 33
       putStrLn "\nSequential version:"
       evalWithTimer $ fib size
       putStrLn "\nVersion using strategies:"
       evalWithTimer $ parfibStrat size
       putStrLn "\nVersion using the Par monad:"
       evalWithTimer $ runPar $ parfibMon size
 
{-# LINE 80 "ParMonad.jhs" #-}
instance MonadZip Par where
        {-# LINE 84 "ParMonad.jhs" #-}
        mzip a b
          = do af <- spawn_ a
               br <- b
               ar <- get af
               return (ar, br)
 
{-# LINE 90 "ParMonad.jhs" #-}
instance MonadZero Par where
        {-# LINE 93 "ParMonad.jhs" #-}
        mzero = block
 
{-# LINE 95 "ParMonad.jhs" #-}
instance MonadAlias Par where
        {-# LINE 99 "ParMonad.jhs" #-}
        malias p = do spawn_ p >>= return . get
 
{-# LINE 102 "ParMonad.jhs" #-}
instance MonadOr Par where
        {-# LINE 106 "ParMonad.jhs" #-}
        morelse a b
          = do r <- newBlocking
               fork (a >>= put_ r)
               fork (b >>= put_ r)
               res <- get r
               return res
 
{-# LINE 117 "ParMonad.jhs" #-}
data Tree a = Leaf a
            | Node (Tree a) (Tree a)
{-# LINE 121 "ParMonad.jhs" #-}
makeTree (x : []) = Leaf x
{-# LINE 122 "ParMonad.jhs" #-}
makeTree list
  = case splitAt ((length list) `div` 2) list of
        (l1, l2) -> Node (makeTree l1) (makeTree l2)
{-# LINE 126 "ParMonad.jhs" #-}
treeLength (Leaf _) = 1
{-# LINE 127 "ParMonad.jhs" #-}
treeLength (Node a b) = treeLength a + treeLength b
 
{-# LINE 129 "ParMonad.jhs" #-}
instance (Show a) => Show (Tree a) where
        {-# LINE 130 "ParMonad.jhs" #-}
        show (Leaf n) = show n
        {-# LINE 131 "ParMonad.jhs" #-}
        show (Node a b) = "(" ++ (show a) ++ ", " ++ (show b) ++ ")"
 
{-# LINE 138 "ParMonad.jhs" #-}
allPrimesSeq :: Tree NumType -> Bool
{-# LINE 139 "ParMonad.jhs" #-}
allPrimesSeq (Leaf a) = isPrime a
{-# LINE 140 "ParMonad.jhs" #-}
allPrimesSeq (Node a b) = allPrimesSeq a && allPrimesSeq b
 
{-# LINE 144 "ParMonad.jhs" #-}
allPrimesPar :: Tree NumType -> Par Bool
{-# LINE 145 "ParMonad.jhs" #-}
allPrimesPar (Leaf num) = return $ isPrime num
{-# LINE 146 "ParMonad.jhs" #-}
allPrimesPar (Node left right)
  = do al' <- spawn_ $ allPrimesPar left
       ar <- allPrimesPar right
       al <- get al'
       return (al && ar)
 
{-# LINE 153 "ParMonad.jhs" #-}
allPrimesJnd :: Tree NumType -> Par Bool
{-# LINE 154 "ParMonad.jhs" #-}
allPrimesJnd (Leaf num) = return $ isPrime num
{-# LINE 155 "ParMonad.jhs" #-}
allPrimesJnd (Node left right)
  = (malias (allPrimesJnd left)) >>=
      (\ avar1 ->
         (malias (allPrimesJnd right)) >>=
           (\ avar2 ->
              (((avar1) `mzip` (avar2) >>=
                  \ jvar1 ->
                    case (jvar1) of
                        (a, b) -> return (return $ a && b)
                        _ -> mzero))
                >>= id))
 
{-# LINE 162 "ParMonad.jhs" #-}
allPrimesShrt :: Tree NumType -> Par Bool
{-# LINE 163 "ParMonad.jhs" #-}
allPrimesShrt (Leaf num) = return $ isPrime num
{-# LINE 164 "ParMonad.jhs" #-}
allPrimesShrt (Node left right)
  = (malias (allPrimesShrt left)) >>=
      (\ avar1 ->
         (malias (allPrimesShrt right)) >>=
           (\ avar2 ->
              (((avar2) >>=
                  \ jvar1 ->
                    case (jvar1) of
                        False -> return (return False)
                        _ -> mzero)
                 `morelse`
                 ((avar1) >>=
                    \ jvar2 ->
                      case (jvar2) of
                          False -> return (return False)
                          _ -> mzero)
                 `morelse`
                 ((avar1) `mzip` (avar2) >>=
                    \ jvar3 ->
                      case (jvar3) of
                          (a, b) -> return (return $ a && b)
                          _ -> mzero))
                >>= id))
 
{-# LINE 174 "ParMonad.jhs" #-}
allPrimesExpl :: Tree NumType -> Par Bool
{-# LINE 175 "ParMonad.jhs" #-}
allPrimesExpl tree
  = do tok <- newCancelToken
       r <- forall' tok tree
       cancel tok
       return r
  where {-# LINE 181 "ParMonad.jhs" #-}
        forall' tok (Leaf num) = return $ isPrime num
        {-# LINE 182 "ParMonad.jhs" #-}
        forall' tok (Node left right)
          = do leftRes <- new
               rightRes <- new
               finalRes <- newBlocking
               forkWith tok
                 (forall' tok left >>= completed leftRes rightRes finalRes)
               forkWith tok
                 (forall' tok right >>= completed rightRes leftRes finalRes)
               get finalRes
        {-# LINE 192 "ParMonad.jhs" #-}
        completed varA varB fin resA
          = do put varA resA
               (if not resA then put fin False else
                  get varB >>= put fin . (&& resA))
{-# LINE 201 "ParMonad.jhs" #-}
mainTree
  = do let {-# LINE 203 "ParMonad.jhs" #-}
           range = [5000000000 .. 5000005000]
       let {-# LINE 204 "ParMonad.jhs" #-}
           primes = [n | n <- range, isPrime n]
       let {-# LINE 205 "ParMonad.jhs" #-}
           tree1 = makeTree primes
       let {-# LINE 206 "ParMonad.jhs" #-}
           tree2 = Node (makeTree primes) (Leaf 256)
       putStrLn $ "Length of tree #1: " ++ (show $ treeLength tree1)
       putStrLn $ "Length of tree #2: " ++ (show $ treeLength tree2)
       putStrLn "\nWithout non-prime number (sequential):"
       evalWithTimer (allPrimesSeq tree1)
       putStrLn "\nWithout non-prime number (parallel):"
       evalWithTimer (runPar $ allPrimesPar tree1)
       putStrLn "\nWithout non-prime number (joinads):"
       evalWithTimer (runPar $ allPrimesJnd tree1)
       putStrLn "\nWith non-prime number (joinads):"
       evalWithTimer (runPar $ allPrimesJnd tree2)
       putStrLn "\nWith non-prime number (shortcircuiting joinads):"
       evalWithTimer (runPar_ $ allPrimesShrt tree2)
       putStrLn "\nWith non-prime number (explicit shortcircuiting):"
       evalWithTimer (runPar $ allPrimesExpl tree2)
{-# LINE 231 "ParMonad.jhs" #-}
main
  = do putStrLn "Note: Program should be executed on multiple"
       putStrLn "processors using 'parmonad +RTS -N2 -RTS'"
       putStrLn "\n-------- Comparing baseline performance --------"
       mainFib
       putStrLn "\n-------- Tree processing using joinads --------"
       mainTree
