{-# OPTIONS_GHC -fwarn-tabs #-}
-- Haskell is space sensitive

{-# OPTIONS_GHC -Wall -Wno-type-defaults -fno-warn-missing-signatures #-}
-- Turn on warnings

--
-- You can start GHCi at the UNIX prompt with the command `stack ghci`.
--
-- You can load this file into GHCi by typing the command `:load Examples.hs` at
-- the GHCi prompt.

module Examples where

type Var = String

data Exp = Var Var
         | Const Double
         | Neg Exp
         | Add Exp Exp
         | Mul Exp Exp
  deriving (Show)

-- | Evaluate expressions
-- eval :: [(Var, Double)] -> Exp -> Maybe -> Double
-- eval env (Var v)     = case lookup v env of
--                          Nothing -> error $ "Unbound variable: " ++ v
--                          Just n  -> n
-- eval _   (Const n)   = Just n
-- eval env (Neg e1)    = eval env e1 * (-1)
-- eval env (Add e1 e2) = eval env e1 + eval env e2
-- eval env (Sub e1 e2) = eval env e1 - eval env e2
-- eval env (Mul e1 e2) = eval env e1 * eval env e2
-- eval env (Div e1 e2) = eval env e1 / eval env e2

-- eval 

-- | Evaluate expressions (Total)
eval :: [(Var, Double)] -> Exp -> Maybe Double
eval env (Var v)     = lookup v env
eval _   (Const n)   = Just n
eval env (Add e1 e2) = do
  n1 <- eval env e1
  n2 <- eval env e2
  return (n1 + n2)
eval env (Mul e1 e2) = do
  n1 <- eval env e1
  n2 <- eval env e2
  return (n1 * n2)
eval env (Neg e)     = do
  n <- eval env e
  return (-n)

-- Compute derivative of an expressions with respect to a variable
deriv :: Exp -> Var -> Exp
deriv (Var v) x
  | v == x          = Const 1
  | otherwise       = Const 0
deriv (Const _) _   = Const 0
deriv (Neg e1) x    = Neg (deriv e1 x)
deriv (Add e1 e2) x = Add (deriv e1 x) (deriv e2 x)
deriv (Mul e1 e2) x = Add (Mul e1 (deriv e2 x))
                          (Mul (deriv e1 x) e2)

-- | "Compile" an expression into a Haskell function
compile :: Exp -> Double -> Maybe Double
compile e x = eval [("x", x)] e

