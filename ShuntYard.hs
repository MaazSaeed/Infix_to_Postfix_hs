module InfixToPostfix (toPostfix) where

import Data.Char
import Data.List

-- Shunting Yard Algorithm

toPostfix :: String -> String
toPostfix str = advance str [] []
  where
    advance str_ queue stack
      | null str_ = queue ++ (reverse stack)
      | isDigit_ ch = advance (tail str_) (queue ++ [ch]) stack
      | isOperator ch = advance (tail str_) (queue ++ snd stk) (fst stk) 
      | isExpo ch = advance (tail str_) queue (stack ++ [ch])
      | isLeftParen ch = advance (tail str_) queue (stack ++ [ch])
      | isRightParen ch = advance (tail str_) (queue ++ snd stk2) (fst stk2)
      where
        ch = head str_
        isDigit_ c = c >= '0' && c <= '9'
        isOperator c = c == '+' || c == '-' || c == '*' || c == '/'
        isExpo c = c == '^'
        isLeftParen c = c == '('
        isRightParen c = c == ')'
        
        stk = stackOps ch stack
        stk2 = stackOps2 stack
        
        
prec :: Char -> Int
prec c
  | c == '+' || c == '-' = 1
  | c == '*' || c == '/' = 2
  | c == '^' = 3
  | c == '(' = 0
  | otherwise = -1
  
stackOps :: Char -> [Char] -> ([Char], [Char])
stackOps ch stck = aux ch (reverse stck) ([], [])
  where
    aux c s acc
      | null s = ([c] ++ reverse (fst acc), snd acc)
      | prec (head s) >= prec c = aux c (tail s) (tail s, snd acc ++ [head s])
      | otherwise = (reverse s ++ [c], snd acc)

stackOps2 :: [Char] -> ([Char], [Char])
stackOps2 stck = aux (reverse stck) ([], [])
  where
    aux s acc
      | null s = (reverse (fst acc), snd acc)
      | head s /= '(' = aux (tail s) (tail s, snd acc ++ [head s])
      | otherwise = (reverse (tail s), snd acc)
