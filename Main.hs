module Main where
import Parsing

-- Based on https://www.youtube.com/watch?v=dDtZLm7HIJs
-- an a bit on https://www.youtube.com/watch?v=N9RUqGYuGfw

data ValNumber
      = ValInt Integer
      | ValDouble Double
      | ValComplex (Double, Double)
      | ValErr [String]
      deriving (Show, Eq)

instance Num ValNumber where
      (+) (ValErr e1) (ValErr e2)     = ValErr $ e1 ++ e2
      (+) (ValErr e1) (_)             = ValErr e1
      (+) (_) (ValErr e2)             = ValErr e2
      (+) (ValInt x) (ValInt y)       = ValInt $ x + y
      (+) (ValInt x) (ValDouble y)    = ValDouble $ fromInteger x + y
      (+) (ValInt x) (ValComplex (r2, i2))    = ValComplex (fromInteger x + r2, i2)
      (+) (ValDouble x) (ValInt y)    = ValDouble $ x + fromInteger y
      (+) (ValDouble x) (ValComplex (r2, i2))    = ValComplex (x + r2, i2)
      (+) (ValDouble x) (ValDouble y) = ValDouble $ x + y
      (+) (ValComplex (r1,i1)) (ValComplex (r2, i2))    = ValComplex (r1 + r2, i1 + i2)
      (+) (ValComplex (r1,i1)) (ValInt y)    = ValComplex (r1 + fromInteger y, i1)
      (+) (ValComplex (r1,i1)) (ValDouble y)    = ValComplex (r1 + y, i1)


      (-) (ValErr e1) (ValErr e2)     = ValErr $ e1 ++ e2
      (-) (ValErr e1) (_)             = ValErr e1
      (-) (_) (ValErr e2)             = ValErr e2
      (-) (ValInt x) (ValInt y)       = ValInt $ x - y
      (-) (ValInt x) (ValDouble y)    = ValDouble $ fromInteger x - y
      (-) (ValInt x) (ValComplex (r2, i2))    = ValComplex (fromInteger x - r2, i2)
      (-) (ValDouble x) (ValInt y)    = ValDouble $ x - fromInteger y
      (-) (ValDouble x) (ValComplex (r2, i2))    = ValComplex (x - r2, i2)
      (-) (ValDouble x) (ValDouble y) = ValDouble $ x - y
      (-) (ValComplex (r1,i1)) (ValComplex (r2, i2))    = ValComplex (r1 - r2, i1 - i2)
      (-) (ValComplex (r1,i1)) (ValInt y)    = ValComplex (r1 - fromInteger y, i1)
      (-) (ValComplex (r1,i1)) (ValDouble y)    = ValComplex (r1 - y, i1)


      (*) (ValErr e1) (ValErr e2)     = ValErr $ e1 ++ e2
      (*) (ValErr e1) (_)             = ValErr e1
      (*) (_) (ValErr e2)             = ValErr e2
      (*) (ValInt x) (ValInt y)       = ValInt $ x * y
      (*) (ValInt x) (ValDouble y)    = ValDouble $ fromInteger x * y
      (*) (ValInt x) (ValComplex (r2, i2))    = ValComplex (fromInteger x * r2, fromInteger x * i2)
      (*) (ValDouble x) (ValInt y)    = ValDouble $ x * fromInteger y
      (*) (ValDouble x) (ValComplex (r2, i2))    = ValComplex (x * r2, x * i2)
      (*) (ValDouble x) (ValDouble y) = ValDouble $ x * y
      (*) (ValComplex (r1,i1)) (ValComplex (r2, i2))    = ValComplex(r1 * r1 - i2 * i2, 2 * r1 * r2)
      (*) (ValComplex (r1,i1)) (ValInt y)    = ValComplex (r1 * fromInteger y, i1 * fromInteger y)
      (*) (ValComplex (r1,i1)) (ValDouble y)    = ValComplex (r1 * y, i1 * y)


      abs (ValErr x) = ValErr x
      abs (ValInt x)    = ValInt $ abs x
      abs (ValDouble x) = ValDouble $ abs x

      signum (ValErr x) = ValErr x
      signum (ValInt x)    = ValInt $ signum x
      signum (ValDouble x) = ValDouble $ signum x

      fromInteger = ValInt
      
instance Fractional ValNumber where -- TODO Complex support
      (/) (ValErr e1) (ValErr e2)               = ValErr $ e1 ++ e2
      (/) (ValErr e1) (_)                       = ValErr e1
      (/) (_) (ValErr e2)                       = ValErr e2
      (/) (ValInt x) (ValInt y)                 = ValDouble $ fromInteger x / fromInteger y
      (/) (ValInt x) (ValDouble y)              = ValDouble $ fromInteger x / y
      (/) (ValDouble x) (ValInt y)              = ValDouble $ x / fromInteger y
      (/) (ValDouble x) (ValDouble y)           = ValDouble $ x / y
      (/) (ValComplex (r1, i1)) (ValInt y)      = ValComplex (r1 / fromInteger y, i1 / fromInteger y)
      (/) (ValComplex (r1, i1)) (ValDouble y)   = ValComplex (r1/y, i1/y)
      (/) (_) (ValComplex y)                     = ValErr ["Div by Complex not supported"]

      fromRational (x)                    = ValDouble $ fromRational x

toDouble :: ValNumber -> Double
toDouble (ValInt x)    = fromInteger x
toDouble (ValDouble x) = x


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
--valImag :: Parser String
valImag = char 'i' <|> char 'j'

valSNumber = valDouble <|> valInt

valComplex :: Parser ValNumber
valComplex = do   x <- valSNumber
                  space
                  valImag
                  space
                  return $ ValComplex (0,toDouble x)
            <|>
            do    space
                  valImag
                  x <- valSNumber
                  space
                  return $ ValComplex (0,toDouble x)
            <|>
            do    space
                  valImag
                  space
                  return $ ValComplex (0,1)

valNumber :: Parser ValNumber
valNumber = valComplex <|> valSNumber

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

printErr (ValErr (x:xs), y) = do printErr (ValErr xs, y)
                                 print x
printErr (ValErr [], y) = print y

--main :: IO ()
main = do   putStrLn "Please enter the expression: "
            line <- getLine
            case parse expr line of
                  [(ValErr x, y)] -> printErr(ValErr x, y)
                  [(ValDouble x, "")] -> print x
                  [(ValInt x, "")] -> print x
                  [(ValComplex (r,i), "")] -> do      putStr . show $ r
                                                      putStr " + "
                                                      putStr . show $ i
                                                      putStrLn " i"
                  _ -> putStrLn "Failed to evaluate expression"
