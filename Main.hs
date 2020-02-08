module Main where
import Parsing

-- Based on https://www.youtube.com/watch?v=dDtZLm7HIJs
-- an a bit on https://www.youtube.com/watch?v=N9RUqGYuGfw

data ValNumber
      = ValInt Integer
      | ValDouble Double
      deriving (Show, Eq)

instance Num ValNumber where
      (+) (ValInt x) (ValInt y)           = ValInt $ x + y
      (+) (ValDouble x) (ValInt y)        = ValDouble $ x + fromInteger y
      (+) (ValInt x) (ValDouble y)        = ValDouble $ fromInteger x + y
      (+) (ValDouble x) (ValDouble y)     = ValDouble $ x + y     
      
      (-) (ValInt x) (ValInt y)           = ValInt $ x - y
      (-) (ValDouble x) (ValInt y)        = ValDouble $ x - fromInteger y
      (-) (ValInt x) (ValDouble y)        = ValDouble $ fromInteger x - y
      (-) (ValDouble x) (ValDouble y)     = ValDouble $ x - y

      (*) (ValInt x) (ValInt y)           = ValInt $ x * y
      (*) (ValDouble x) (ValInt y)        = ValDouble $ x * fromInteger y
      (*) (ValInt x) (ValDouble y)        = ValDouble $ fromInteger x * y
      (*) (ValDouble x) (ValDouble y)     = ValDouble $ x * y
      
      abs (ValInt x) = ValInt $ abs x
      abs (ValDouble x) = ValDouble $ abs x

      signum (ValInt x) = ValInt $ signum x
      signum (ValDouble x) = ValDouble $ signum x

      fromInteger = ValInt
instance Fractional ValNumber where
      (/) (ValInt x) (ValInt y)           = ValDouble $ fromInteger x / fromInteger y
      (/) (ValDouble x) (ValInt y)        = ValDouble $ x / fromInteger y
      (/) (ValInt x) (ValDouble y)        = ValDouble $ fromInteger x / y
      (/) (ValDouble x) (ValDouble y)     = ValDouble $ x / y

      fromRational (x)                    = ValDouble $ fromRational x

toDouble :: ValNumber -> Double
toDouble (ValInt x)           = fromInteger x
toDouble (ValDouble x)        = x


valNat :: Parser ValNumber
valNat = do xs <- some digit
            return $ ValInt $ read xs

valInt :: Parser ValNumber
valInt = do char '-'
            n <- valNat
            return (-n)
            <|> valNat

valposdouble :: Parser ValNumber
valposdouble = do xs <- some (digit)
                  y  <- char '.'
                  ys <- some (digit)
                  return $ ValDouble (read (xs ++ [y] ++ ys))

valDouble :: Parser ValNumber
valDouble = do    space
                  char '-'
                  x <- valposdouble
                  space
                  return (-x)
            <|>
            do    space
                  x <- valposdouble
                  space
                  return x

valNumber :: Parser ValNumber
valNumber = valDouble <|> valInt

expr :: Parser ValNumber
expr = do   x <- term
            char '+'
            y <- expr
            return $ x + y
      <|> 
      do    x <- term
            char '-'
            y <- exprNeg
            return $ x - y
      <|> term

-- Magic to handle concurrent subtraction correctly (emulate evaluating left to right)
-- Really hacky but I don't know Haskell well enough to do it better
exprNeg :: Parser ValNumber
exprNeg = 
      do    x <- term  
            char '-'
            y <- exprNeg
            return $ x + y
      <|> 
      do    x <- term
            char '+'
            y <- expr
            return $ x - y
      <|> term

term :: Parser ValNumber
term = do   x <- factor
            char '*'
            y <- term
            return (x*y)
      <|> 
      do    x <- factor
            char '/'
            y <- term
            return (x / y)
      <|> factor

factor :: Parser ValNumber
factor = valNumber
      <|> 
      do    space
            char '(' 
            x <- expr
            char ')'
            space
            return x
      <|>
      funct
funct :: Parser ValNumber
funct = do  space
            string "sin("
            x <- expr
            char ')'
            space
            return $ ValDouble $ sin $ toDouble x
      <|>
      do    space
            string "cos("
            x <- expr
            char ')'
            space
            return $ ValDouble $ cos $ toDouble x
      <|>
      do    space
            string "tan("
            x <- expr
            char ')'
            return $ ValDouble $ tan $ toDouble x
      <|>
      do    space
            string "sin^"
            x <- expr
            char '('
            y <- expr
            char ')'
            space
            return $ ValDouble $ sin (toDouble x) ** toDouble y
      <|>
      do    space
            string "cos^"
            x <- expr
            char '('
            y <- expr
            char ')'
            space
            return $ ValDouble $ cos (toDouble x) ** toDouble y
      <|>
      do    space
            string "tan^"
            x <- expr
            char '('
            y <- expr
            char ')'
            space
            return $ ValDouble $ tan (toDouble x) ** toDouble y
      <|>
      do    space
            string "asin("
            x <- expr
            char ')'
            space
            return $ ValDouble $ asin $ toDouble x
      <|>
      do    space
            string "acos("
            x <- expr
            char ')'
            space
            return $ ValDouble $ acos $ toDouble x
      <|>
      do    space
            string "atan("
            x <- expr
            char ')'
            space
            return $ ValDouble $ atan $ toDouble x
      <|>
      do    space
            string "root("
            x <- expr
            char ')'
            space
            return $ ValDouble $ sqrt $ toDouble x
      <|>
      do    space
            string "root_"
            x <- expr
            char '('
            y <- expr
            char ')'
            space
            return $ ValDouble $ toDouble y ** (1 / toDouble x)
            --return $ y ** ( 1 / x)
      <|>
      do    space
            string "log("
            x <- expr
            char ')'
            space
            return $ ValDouble $ log $ toDouble x
      <|>
      do    space
            string "log_"
            x <- expr
            char '('
            y <- expr
            char ')'
            space
            return $ ValDouble $ logBase (toDouble x) (toDouble y)
      <|>
      do    space
            string "exp("
            x <- expr
            char ')'
            space
            return $ ValDouble $ exp $ toDouble x
      <|>
      do    space
            string "pi"
            space
            return $ ValDouble pi
      <|>
      do    space
            string "e"
            space
            return $ ValDouble $ exp 1

--main :: IO ()
main = do   putStrLn "Please enter the expression: "
            line <- getLine
            case parse expr line of
                  [(ValDouble x, "")] -> print x
                  [(ValInt x, "")] -> print x
                  --[(x, "")] -> print x
                  _ -> putStrLn "Failed to evaluate expression"
