{-# LANGUAGE LambdaCase #-}

module Untyped where

import Control.Monad.Except
import Data.Map as Map

data Expression
  = Literal Value
  | Variable String
  | UnOp UnOp Expression
  | BinOp BinOp Expression Expression
  | Let String Expression Expression
  deriving Show

data SimpleExpression
  = SLiteral Value
  | SUnOp UnOp SimpleExpression
  | SBinOp BinOp SimpleExpression SimpleExpression
  deriving Show

data Value
  = VBool Bool
  | VInt Int
  | VDouble Double
  deriving Show

data UnOp
  = Not
  | Abs
  deriving Show

data BinOp
  = And
  | Or
  | Equ
  | Neq
  | Gt
  | Gte
  | Lt
  | Lte
  | Add
  | Sub
  | Mul
  deriving Show

type Context = Map String SimpleExpression

simplifyExpression :: Context -> Expression -> Except String SimpleExpression
simplifyExpression ctx = \ case
  Literal v         -> return $ SLiteral v
  Variable name     -> case Map.lookup name ctx of
    Nothing -> throwError $ "variable " ++ name ++ " out of scope"
    Just x  -> return x
  UnOp unOp e       -> SUnOp unOp <$> simplifyExpression ctx e
  BinOp binOp e1 e2 -> SBinOp binOp
    <$> simplifyExpression ctx e1
    <*> simplifyExpression ctx e2
  Let name e e'     -> do
    sExp <- simplifyExpression ctx e
    let ctx' = Map.insert name sExp ctx
    simplifyExpression ctx' e'
