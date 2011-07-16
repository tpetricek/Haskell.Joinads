{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MonadComprehensions #-}

import Data.Char
import Control.Monad
import Control.Monad.Zip
import Control.Applicative (Applicative, pure, (<*>))

-------------------------------------------------------------------------------
-- Simple parser combinators
-- (Non-standard feature: result also records the number of consumed
-- characters, which is used by the implementation of `mzip`)
-------------------------------------------------------------------------------

newtype Parser a 
	= Parser (String -> [(a, Int, String)])

instance Monad Parser where
  return a = Parser (\input -> [(a, 0, input)])
  (Parser p1) >>= f = Parser (\input ->
    [ (result, n1 + n2, tail) 
        | (a, n1, input') <- p1 input
        , let (Parser p2) = f a
        , (result, n2, tail) <- p2 input' ])

instance MonadPlus Parser where 
  mzero = Parser (\_ -> [])
  mplus (Parser p1) (Parser p2) = Parser (\input ->
    p1 input ++ p2 input)

run (Parser p) input = 
  [ result | (result, _, tail) <- p input, tail == "" ]

-------------------------------------------------------------------------------
-- Simple parser combinators
-------------------------------------------------------------------------------

item :: Parser Char
item = Parser (\input -> case input of
  "" -> []
  c:cs -> [(c, 1, cs)])

sat p = [ c | c <- item, p c ]
char c = sat (c ==)
notChar c = sat (c /=)

many p = many1 p `mplus` return []
many1 p = [ a:as | a <- p, as <- many p ] 

-- Equivalents using the do-notation:
--  sat p = do c <- item; if p c then return c else mzero
--  many1 p = do a <- p; as <- many p; return $ a:as 

-------------------------------------------------------------------------------

brackets :: Char -> Char -> Parser a -> Parser a
brackets open close body = 
  [ inner | _ <- char open
          , inner <- (brackets open close body) `mplus` body --(many $ notChar close)
          , _ <- char close ]

-- Equivalent using the do-notation:
--  brackets open close = do 
--    _ <- char open;
--    n <- brackets open close `mplus` (many (notChar close))
--    _ <- char close;
--    return n

skipBrackets = brackets '(' ')' (many item)

skipAllBrackets = brackets '(' ')' body
  where body = many [c | c <- notChar '(' | _ <- notChar ')' ]

-------------------------------------------------------------------------------
-- Adding `mzip` 
-------------------------------------------------------------------------------

instance MonadZip Parser where 
  mzip (Parser p1) (Parser p2) = Parser (\input -> 
    [ ((a, b), n1, tail1) 
        | (a, n1, tail1) <- p1 input
        , (b, n2, tail2) <- p2 input
        , n1 == n2 ])

notContextFree = 
  [ length s 
      | s <- many $ char 'x', _ <- brackets 'y' 'z' unit
      | _ <- brackets 'x' 'y' unit, _ <- many $ char 'z' ]
    where unit = return ()

-------------------------------------------------------------------------------
-- Adding Applicative
-------------------------------------------------------------------------------

instance Functor Parser where
  fmap f v = [ f a | a <- v ]

instance Applicative Parser where
  pure a = return a
  fs <*> as = [ f a | f <- fs, a <- as ]

-- Applicative version of 'brackets'

bracketsA :: Char -> Char -> Parser a -> Parser a
bracketsA op cl body = 
  pure (\_ inner _ -> inner)
    <*> char op
    <*> bracketsA op cl body `mplus` body
    <*> char cl

skipBracketsA = bracketsA '(' ')' (many item)

-------------------------------------------------------------------------------
-- Using `mzip` for input validation
-------------------------------------------------------------------------------

string "" = return ""
string (s:ss) = do s <- char s; ss <- string ss; return $ s:ss
numeric = many (sat isDigit)
length9 = times item 9
validPhone1 = (numeric `mzip` length9) >> (return True)
startsWith p = p >> many item

times p 0 = return []
times p n = do a <- p; as <- times p (n - 1); return $ a:as

validPhone = 
  [ num | num <- many (sat isDigit)
        | _   <- times item 10 
        | _   <- startsWith (string "1223") ]