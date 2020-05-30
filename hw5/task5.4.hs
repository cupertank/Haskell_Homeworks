import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

-- Общий прицнип заключается в построении AST при помощи Parsec, дифференецирование дерева, оптимизация дерева, а потом красивый вывод

-- Type


data Expr = Var
          | Num Int
          | Add Expr Expr
          | Minus Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Neg Expr
          deriving (Show)

instance Eq Expr where
  Var == Var = True
  (Num x) == (Num y) = x == y
  (Add x y) == (Add z t) = x == z && y == t || x == t && y == z
  (Minus x y) == (Minus z t) = x == z && y == t
  (Mul x y) == (Mul z t) = x == z && y == t || x == t && y == z
  (Div x y) == (Div z t) = x == z && y == t
  (Neg x) == (Neg y) = x == y
  _ == _ = False


-- Parser (Using Parsec)

expr = buildExpressionParser table factor 
       <?> "expression"

factor = do
  _    <- char '('
  expression <- expr
  _    <- char ')'
  return expression
  <|> variable

variable = do
  _ <- char 'x'
  return Var
  <|> number

number = do
  num <- many1 digit
  let int = read num
  return (Num int)

table   = [ 
            [prefix "-" Neg ],
            [binary "*" Mul AssocLeft, binary "/" Div AssocLeft ],
            [binary "+" Add AssocLeft, binary "-" Minus AssocLeft ]
          ]


binary  name fun assoc = Infix (string name >> return fun) assoc
prefix  name fun       = Prefix (string name >> return fun )
postfix name fun       = Postfix (string name >> return fun )

-- derivative rules f

derivative (Mul left right) = Add (Mul (derivative left) right) (Mul left (derivative right))
derivative (Add left right) = Add (derivative left) (derivative right)
derivative (Minus left right) = Minus (derivative left) (derivative right)
derivative (Div top down) = Div (Minus (Mul (derivative top) down) (Mul top (derivative down))) (Mul down down)
derivative (Neg x) = Neg (derivative x)
derivative (Num _) = Num 0
derivative Var = Num 1

-- optimizer

optimizer' (Mul left (Num 1)) = optimizer' left
optimizer' (Mul (Num 1) right) = optimizer' right
optimizer' (Mul _ (Num 0)) = Num 0
optimizer' (Mul (Num 0) _) = Num 0
optimizer' (Mul (Num x) (Num y)) = Num (x * y)
optimizer' (Mul left right) = Mul left right

optimizer' (Div top (Num 1)) = optimizer' top
optimizer' (Div (Num 0) _) = Num 0
optimizer' (Div (Num x) (Num y)) = Num (div x y)
optimizer' (Div Var Var) = Num 1
optimizer' (Div left right)
                          | left == right = Num 1
                          | otherwise     = Div left right

optimizer' (Add (Num 0) right) = optimizer' right
optimizer' (Add left (Num 0)) = optimizer' left
optimizer' (Add (Num x) (Num y)) = Num (x + y)
optimizer' (Add left right)
                          | left == right = Mul (Num 2) left
                          | otherwise     = Add left right 

optimizer' (Minus left (Num 0)) = optimizer' left
optimizer' (Minus (Num 0) right) = Neg (optimizer' right)
optimizer' (Minus (Num x) (Num y)) = Num (x-y)
optimizer' (Minus left right)
                          | left == right = Num 0
                          | otherwise     = Minus left right

optimizer' (Neg (Num x)) = Num ((-1) * x)
optimizer' (Neg x) = Neg x
optimizer' Var = Var
optimizer' (Num x) = Num x

optimizer Var = Var
optimizer (Num x) = Num x
optimizer (Minus left right) = optimizer' (Minus (optimizer left) (optimizer right))
optimizer (Add left right) = optimizer' (Add (optimizer left) (optimizer right))
optimizer (Div top down) = optimizer' (Div (optimizer top) (optimizer down))
optimizer (Mul left right) = optimizer' (Mul (optimizer left) (optimizer right))
optimizer (Neg x) = optimizer' (Neg (optimizer x))

-- unparser
priority :: Expr -> Int
priority x = case x of
  Var        -> 0
  Num _      -> 0
  Neg _      -> 1
  Mul _ _    -> 2
  Div _ _    -> 2
  Add _ _    -> 3
  Minus _ _  -> 3


unparse Var = "x"
unparse (Num x) = show x
unparse (Add left right) = (if priority left >= 3 then "(" ++ unparse left ++ ")" else unparse left) ++ "+" ++
                           (if priority right >= 3 then "(" ++ unparse right ++ ")" else unparse right)

unparse (Minus left right) = (if priority left >= 3 then "(" ++ unparse left ++ ")" else unparse left) ++ "-" ++
                             (if priority right >= 3 then "(" ++ unparse right ++ ")" else unparse right)

unparse (Mul left right) = (if priority left >= 2 then "(" ++ unparse left ++ ")" else unparse left) ++ "*" ++
                           (if priority right >= 2 then "(" ++ unparse right ++ ")" else unparse right)

unparse (Div left right) = (if priority left >= 2 then "(" ++ unparse left ++ ")" else unparse left) ++ "/" ++
                         (if priority right >= 2 then "(" ++ unparse right ++ ")" else unparse right)

unparse (Neg x) = "-" ++ if priority x >= 1 then "(" ++ unparse x ++ ")" else unparse x

-- main program
program input = do
  let parsed = parse expr "" input
  case parsed of
    (Left err)   -> print err
    (Right tree) -> print (unparse (optimizer (derivative tree)))

-- test

test1 = program "x*x"
test2 = program "-5*41+(45/5)*x-(14*88)x*x"