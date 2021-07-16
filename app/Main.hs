module Main where

import Data.Char
import System.IO

data Token
  = Lit Int
  | Op Op
  | Paren Bool
  deriving (Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)

data Ast
  = Const Int
  | UnOp Op Ast
  | BinOp Op Ast Ast

instance Show (Ast) where
  show (Const a) = show a
  show (UnOp o v) = show o ++ "\n" ++ (unlines $map ("  " ++) $lines $show v)
  show (BinOp o v1 v2) =
    show o ++
    "\n" ++
    (unlines $map ("  " ++) $lines $show v1) ++
    (unlines $map ("  " ++) $lines $show v2)

maybeAdd :: Maybe a -> [a] -> [a]
maybeAdd (Just x) xs = x : xs
maybeAdd Nothing xs = xs

tokenize :: String -> Either String [Token]
tokenize input = reverse <$> go input ([], Nothing)
  where
    go :: String -> ([Token], Maybe Token) -> Either String [Token]
    go "" (ts, t) = Right (maybeAdd t ts)
    go (x:xs) (ts, t)
      | x == ' ' = go xs ((maybeAdd t ts), Nothing)
      | x == '/' = go xs (Op Div : (maybeAdd t ts), Nothing)
      | x == '*' = go xs (Op Mul : (maybeAdd t ts), Nothing)
      | x == '+' = go xs (Op Add : (maybeAdd t ts), Nothing)
      | x == '-' = go xs (Op Sub : (maybeAdd t ts), Nothing)
      | x == '(' = go xs (Paren True : (maybeAdd t ts), Nothing)
      | x == ')' = go xs (Paren False : (maybeAdd t ts), Nothing)
      | isDigit x =
        case t of
          Just (Op o) -> go xs ((Op o : ts), Just $ Lit (digitToInt x))
          Nothing -> go xs (ts, Just $ Lit (digitToInt x))
          Just (Lit o) -> go xs (ts, Just $ Lit ((digitToInt x) + o * 10))
      | otherwise = Left ("Unexpected symbol " ++ show x)

data ParseTok
  = Raw Token
  | Lifted Int Ast
  deriving (Show)

parse :: [ParseTok] -> [Token] -> Either String Ast
parse [Lifted _ ast] [] = Right ast
parse ((Raw (Paren False)):(Lifted _ b):(Raw (Paren True)):state) input =
  parse ((Lifted 1 b) : state) input

parse ((Lifted 2 b):(Raw (Op Mul)):(Lifted 2 a):state) input =
  parse ((Lifted 2 $ BinOp Mul a b) : state) input

parse ((Lifted 4 b):(Raw (Op Div)):(Lifted 4 a):state) input =
  parse ((Lifted 4 $ BinOp Div a b) : state) input

parse ((Lifted 3 b):(Raw (Op Add)):(Lifted 3 a):state) input =
  parse ((Lifted 3 $ BinOp Add a b) : state) input

parse ((Lifted 3 b):(Raw (Op Sub)):(Lifted 3 a):state) input =
  parse ((Lifted 3 $ BinOp Sub a b) : state) input

parse state@(Lifted 3 e:_) (Op Add:input) = parse ((Raw $ Op Add) : state) input
parse state@(Lifted 3 e:_) (Op Sub:input) = parse ((Raw $ Op Sub) : state) input
parse state@(Lifted 2 e:_) (Op Mul:input) = parse ((Raw $ Op Mul) : state) input
parse state@(Lifted 4 e:_) (Op Div:input) = parse ((Raw $ Op Div) : state) input

parse state (Lit a:input) = parse ((Lifted 1 (Const a)) : state) input

parse state (Paren True:input) = parse ((Raw $ Paren True) : state) input

parse state@((Lifted a e):(Raw (Paren True)):_) (Paren False:input) =
  parse ((Raw $ Paren False) : state) input

parse (Lifted a e:state) input = parse (Lifted (a + 1) e : state) input
parse state [] = Left $ "Parsing Failed: The stack state is " ++ show state
parse state input =
  Left $
  "Parsing non-exhaustive, this is a bug, please report it\n  The stack state is: " ++
  show state ++ "\n  The input left is: " ++ show input

reduce :: Ast -> Either String Int
reduce (Const a) = Right a
reduce (UnOp Sub ast) = (0 -) <$> reduce ast
reduce (UnOp Add ast) = reduce ast
reduce (BinOp Mul lhs rhs) = (*) <$> reduce lhs <*> reduce rhs
reduce (BinOp Add lhs rhs) = (+) <$> reduce lhs <*> reduce rhs
reduce (BinOp Sub lhs rhs) = (-) <$> reduce lhs <*> reduce rhs
reduce (BinOp Div lhs rhs) =
  reduce rhs >>= \x ->
    case x of
      0 -> Left "Division by zero!"
      rhs -> (`div` rhs) <$> reduce lhs

solve :: String -> String
solve s = show (pure s >>= tokenize >>= parse [] >>= reduce)

main :: IO ()
main = hGetContents stdin >>= putStrLn . unlines . map solve . lines
