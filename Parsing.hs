-- Parsing library by 
-- from Computerphile Video
-- https://www.youtube.com/watch?v=dDtZLm7HIJs


module Parsing (module Parsing, module Control.Applicative) where 

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse (P p) = p

item = P (\inp -> case inp of
                        []          -> []
                        (x:xs)      -> [(x,xs)])


instance Functor Parser where
    fmap g p = P (\inp -> case parse p inp of
                            []          -> []
                            [(v,out)]   -> [(g v, out)])

instance Applicative Parser where
    pure v      = P (\inp -> [(v, inp)])
    pg <*> px   = P (\inp -> case parse pg inp of 
                                []          -> []
                                [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
    p >>= f = P (\inp -> case parse p inp of
                              []          -> []
                              [(v,out)]   -> parse (f v) out)

instance Alternative Parser where
    empty = P (const [])
    p <|> q = P (\inp -> case parse p inp of
                            []          -> parse q inp
                            [(v,out)]   -> [(v,out)])

sat p = do x <- item
           if p x then return x else empty

digit = sat isDigit

lower = sat isLower

upper = sat isUpper

letter = sat isAlpha

alphanum = sat isAlphaNum

char x = sat (== x)

string [] = return []
string (x:xs) = do  char x
                    string xs
                    return (x:xs)

ident = do  x <- lower
            xs <- many alphanum
            return (x:xs)

nat = do xs <- some digit
         return (read xs :: Integer)

int = do char '-'
         n <- nat
         return (-n)
         <|> nat

posdouble = do  xs <- some (digit)
                y  <- char '.'
                ys <- some (digit)
                return (read (xs ++ [y] ++ ys) :: Double)
            <|>
            do  xs <- some (digit)
                return (read xs)

double = do space
            char '-'
            x <- posdouble
            space
            return (-x)
        <|>
        do  space
            x <- posdouble
            space
            return x

space = do many (sat isSpace)
           return ()

token p = do space
             v <- p
             space
             return v

identifier = token ident

natural = token nat

integer = token int

symbol xs = token (string xs)

