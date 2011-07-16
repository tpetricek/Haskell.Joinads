{-# LINE 1 "Parsers.jhs" #-}
module Main (main) where
{-# LINE 1 "Parsers.jhs" #-}
import Data.Char
{-# LINE 2 "Parsers.jhs" #-}
import Control.Applicative (Applicative, pure, (<*>))
{-# LINE 3 "Parsers.jhs" #-}
import Control.Monad.Joinads
 
{-# LINE 11 "Parsers.jhs" #-}
newtype Parser a = Parser (String -> [(a, Int, String)])
 
{-# LINE 18 "Parsers.jhs" #-}
instance Monad Parser where
        {-# LINE 20 "Parsers.jhs" #-}
        return a = Parser (\ input -> [(a, 0, input)])
        {-# LINE 24 "Parsers.jhs" #-}
        (Parser p1) >>= f
          = Parser
              (\ input ->
                 [(result, n1 + n2, tail) | (a, n1, input') <- p1 input,
                  let {-# LINE 27 "Parsers.jhs" #-}
                      (Parser p2) = f a,
                  (result, n2, tail) <- p2 input'])
 
{-# LINE 30 "Parsers.jhs" #-}
instance MonadZero Parser where
        {-# LINE 32 "Parsers.jhs" #-}
        mzero = Parser (\ _ -> [])
 
{-# LINE 34 "Parsers.jhs" #-}
instance MonadOr Parser where
        {-# LINE 38 "Parsers.jhs" #-}
        morelse (Parser p1) (Parser p2)
          = Parser
              (\ input ->
                 case p1 input of
                     [] -> p2 input
                     x -> x)
 
{-# LINE 43 "Parsers.jhs" #-}
instance MonadPlus Parser where
        {-# LINE 47 "Parsers.jhs" #-}
        mplus (Parser p1) (Parser p2)
          = Parser (\ input -> p1 input ++ p2 input)
 
{-# LINE 50 "Parsers.jhs" #-}
instance MonadZip Parser where
        {-# LINE 55 "Parsers.jhs" #-}
        mzip (Parser p1) (Parser p2)
          = Parser
              (\ input ->
                 [((a, b), n1, tail1) | (a, n1, tail1) <- p1 input,
                  (b, n2, tail2) <- p2 input, n1 == n2])
 
{-# LINE 62 "Parsers.jhs" #-}
instance MonadAlias Parser where
        {-# LINE 63 "Parsers.jhs" #-}
        malias = return
{-# LINE 69 "Parsers.jhs" #-}
run (Parser p) input
  = [result | (result, _, tail) <- p input, tail == ""]
 
{-# LINE 72 "Parsers.jhs" #-}
item :: Parser Char
{-# LINE 73 "Parsers.jhs" #-}
item
  = Parser
      (\ input ->
         case input of
             "" -> []
             c : cs -> [(c, 1, cs)])
{-# LINE 77 "Parsers.jhs" #-}
sat p
  = do c <- item
       if p c then return c else mzero
{-# LINE 78 "Parsers.jhs" #-}
char c = sat (c ==)
{-# LINE 79 "Parsers.jhs" #-}
notChar c = sat (c /=)
{-# LINE 81 "Parsers.jhs" #-}
string "" = return ""
{-# LINE 82 "Parsers.jhs" #-}
string (s : ss)
  = do s <- char s
       ss <- string ss
       return $ s : ss
{-# LINE 84 "Parsers.jhs" #-}
numeric = many (sat isDigit)
{-# LINE 85 "Parsers.jhs" #-}
length9 = times item 9
{-# LINE 86 "Parsers.jhs" #-}
startsWith p = p >> many item
{-# LINE 88 "Parsers.jhs" #-}
times p 0 = return []
{-# LINE 89 "Parsers.jhs" #-}
times p n
  = do a <- p
       as <- times p (n - 1)
       return $ a : as
{-# LINE 91 "Parsers.jhs" #-}
many p = many1 p `mplus` return []
{-# LINE 92 "Parsers.jhs" #-}
many1 p
  = do a <- p
       as <- many p
       return $ a : as
 
{-# LINE 100 "Parsers.jhs" #-}
brackets :: Char -> Char -> Parser a -> Parser a
{-# LINE 101 "Parsers.jhs" #-}
brackets open close body
  = do _ <- char open
       n <- brackets open close body `mplus` body
       _ <- char close
       return n
{-# LINE 108 "Parsers.jhs" #-}
skipBrackets = brackets '(' ')' (many body)
  where {-# LINE 109 "Parsers.jhs" #-}
        body = item
{-# LINE 113 "Parsers.jhs" #-}
skipAllBrackets = brackets '(' ')' (many body)
  where {-# LINE 114 "Parsers.jhs" #-}
        body
          = (malias (notChar '(')) >>=
              (\ avar1 ->
                 (malias (notChar ')')) >>=
                   (\ avar2 ->
                      (((avar1) `mzip` (avar2) >>=
                          \ jvar1 ->
                            case (jvar1) of
                                (c, _) -> return (return c)
                                _ -> mzero))
                        >>= id))
{-# LINE 117 "Parsers.jhs" #-}
validPhone
  = (malias (many (sat isDigit))) >>=
      (\ avar1 ->
         (malias (times item 10)) >>=
           (\ avar2 ->
              (malias (startsWith (string "1223"))) >>=
                (\ avar3 ->
                   (((avar1) `mzip` (avar2) `mzip` (avar3) >>=
                       \ jvar1 ->
                         case (jvar1) of
                             ((num, _), _) -> return (return num)
                             _ -> mzero))
                     >>= id)))
{-# LINE 123 "Parsers.jhs" #-}
main
  = do putStrLn "Skip brackets"
       putStrLn $ show $ run skipBrackets "((hello))"
       putStrLn "Skip all brackets"
       putStrLn $ show $ run skipAllBrackets "((hello))"
       putStrLn "Valid phone? No"
       putStrLn $ show $ run validPhone "1221445566"
       putStrLn "Valid phone? Yes"
       putStrLn $ show $ run validPhone "1223445566"
