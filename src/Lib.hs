module Lib
    ( opExprAssign,
      isExprOperator,
      splitStrNumbers,
      isValidInfixStringList,
      combineUnaryOperatorsWithNumbers,
      combineUnaryOperators,
      isOperand
    ) where

import Data.Char (isNumber, isSpace)
import Control.Monad.State -- Used for Shutting Yard algorithm

-- Expression data type for calculator
data Expr = Number Double
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show, Read)

-- Aliases for Infix Calculator
type IsRightAssociative = Bool
type Precedence = Int
type InfixString = String

operators :: String
operators   = "+-*/^" -- + and - are unary operators as well

isOperator :: Char -> Bool
isOperator x = x `elem` operators

isOperand :: InfixString -> Bool
isOperand str =
  case reads str :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False

opExprAssign :: (Monad m, MonadFail m) => Expr -> m (Precedence, IsRightAssociative) -- Maybe (Int, Bool)
opExprAssign (Add _ _) = return (2,False)
opExprAssign (Sub _ _) = return (2,False)
opExprAssign (Mul _ _) = return (3,False)
opExprAssign (Div _ _) = return (3,False)
opExprAssign badExpr   = fail $ "The expression: " ++ (show badExpr) ++ " is not valid"

isExprOperator :: (Monad m, MonadFail m) => Expr -> m Bool
isExprOperator (Add _ _) = return True
isExprOperator (Sub _ _) = return True
isExprOperator (Mul _ _) = return True
isExprOperator (Div _ _) = return True
isExprOperator badExpr   = fail $ "The expression: " ++ (show badExpr) ++ " is not valid"

-- Assumes that operators are of length 1,
-- unary operators will need to be computed before this is called
isValidInfixStringList :: [InfixString] -> Bool
isValidInfixStringList [] = True
isValidInfixStringList [_,_] = False -- cannot be infix with two elements
isValidInfixStringList list@(x:_)
 | x == "*" || x == "/" = False -- non unary operators
 | otherwise = countBrackets list 0 0 && isValidInfixStringList' (combineUnaryOperatorsWithNumbers $ combineUnaryOperators list)
 where isValidInfixStringList' :: [InfixString] -> Bool
       isValidInfixStringList' [] = False
       isValidInfixStringList' [")"] = True -- matched brackets therefore if last element ), True
       isValidInfixStringList' [y] = isOperand y
       isValidInfixStringList' (y:ys)
        | (length y == 1 && isOperator (head y)) &&
                         (firstOperandElem  || headYS == "(") = isValidInfixStringList' ys
        | isOperand y && (firstOperatorElem || headYS == ")") = isValidInfixStringList' ys
        | y == ")"    && (firstOperatorElem || headYS == ")") = isValidInfixStringList' ys
        | y == "("    && (firstOperandElem  || headYS == "(") = isValidInfixStringList' ys
        | otherwise = False
         where firstOperandElem  = isOperand . head $ ys
               firstOperatorElem = isOperator . head . head $ ys
               headYS = head ys

countBrackets :: [String] -> Int -> Int -> Bool
countBrackets [] open close = open == close
countBrackets (x:xs) open close
 | x == "("  = countBrackets xs (open+1) close
 | x == ")"  = countBrackets xs open (close+1)
 | otherwise = countBrackets xs open close

splitStrNumbers :: InfixString -> [InfixString]
splitStrNumbers xs =  filter (/= "") (splitStrNumbers' xs)
 where splitStrNumbers' :: InfixString -> [InfixString]
       splitStrNumbers' [] = []
       splitStrNumbers' zs =
         case span (isDottedNumber) zs of
           (ns,nss) -> ns :
             case span (not . isNumber) nss of
               (y,ys) -> map (\c -> [c]) y ++ splitStrNumbers' ys
        where isDottedNumber :: Char -> Bool
              isDottedNumber c = isNumber c || c == '.'


-- If the first element of a head is +,- then it needs to have an operand on the left
-- 0 is the identity element for both addition and negation of natural numbers,
-- and since it's unary we will put 0 in front
-- Also combine the the unary operators with numbers if something like 3 * - 3 is given
combineUnaryOperatorsWithNumbers :: [InfixString] -> [InfixString]
combineUnaryOperatorsWithNumbers [] = []
combineUnaryOperatorsWithNumbers [x] = [x]
combineUnaryOperatorsWithNumbers list@(x:y:xs)
 | (length x == 1 && isOperator (head x)) && x == "+" &&
    (isOperand y || y == "(") = combineUnaryOperatorsWithNumbers' (y:xs)
 | (length x == 1 && isOperator (head x)) && x == "-" &&
    (isOperand y || y == "(") = "-1" : "*" : combineUnaryOperatorsWithNumbers' (y:xs)
 | otherwise = combineUnaryOperatorsWithNumbers' list
 where combineUnaryOperatorsWithNumbers' :: [InfixString] -> [InfixString]
       combineUnaryOperatorsWithNumbers' [] = []
       combineUnaryOperatorsWithNumbers' [a] = [a]
       combineUnaryOperatorsWithNumbers' (a:b:bs)
        | a == "(" && (length b == 1 &&
           isOperator (head b) && b == "+") = a : combineUnaryOperatorsWithNumbers' bs
        | a == "(" && (length b == 1 &&
           isOperator (head b) && b == "-") = "-1" : "*" : b : combineUnaryOperatorsWithNumbers' bs
        | (length a == 1 && isOperator (head a)) && (length b == 1 &&
           isOperator (head b) && b == "+") = a : combineUnaryOperatorsWithNumbers' bs
        | (length a == 1 && isOperator (head a)) && (length b == 1 &&
           isOperator (head b) && b == "-") = a : "-1" : "*" : combineUnaryOperatorsWithNumbers' bs
        | otherwise = a : combineUnaryOperatorsWithNumbers' (b:bs)

-- Unary operators are +,-
combineUnaryOperators :: [InfixString] -> [InfixString]
combineUnaryOperators [] = []
combineUnaryOperators [x] = [x]
combineUnaryOperators (x:y:xs)
 | isOperand x  = x : combineUnaryOperators (y:xs)
 | minusRule = combineUnaryOperators ("-":xs)
 | plusRule  = combineUnaryOperators ("+":xs)
 | y /= "+" && y /= "-" = x : y : combineUnaryOperators xs
 | otherwise  =  x : combineUnaryOperators (y:xs)
 where minusRule = x == "+" && y == "-" || x == "-" && y == "+"
       plusRule  = x == "+" && y == "+" || x == "-" && y == "-"
