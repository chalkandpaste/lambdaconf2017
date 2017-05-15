{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval (evaluate) where

import Typed hiding (addToContext, lookupContext)

import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Map.Strict as Map
import Data.Maybe


type EvalContext = Map String (A TExpr)

emptyContext :: EvalContext
emptyContext = Map.empty

type EvalM a = State EvalContext a

lookupContext :: String -> EvalM (A TExpr)
lookupContext ident = fromJust . Map.lookup ident <$> get

addToContext :: String -> A TExpr -> EvalM ()
addToContext ident expr = put =<< Map.insert ident expr <$> get

evaluate :: TExpr ty -> ty
evaluate texpr = evalState (evaluate texpr) emptyContext

evaluate' :: forall ty. TExpr ty -> EvalM ty
evaluate' = \case
  Literal v           -> value v
  (Variable ident :: TExpr ty)     -> undefined -- do
    {-A (texpr :: TExpr ty) <- lookupContext ident-}
    {-evaluate' texpr-}
  UnOp unOp expr      -> evaluate'UnOp unOp expr
  BinOp binOp e1 e2   -> evaluate'BinOp binOp e1 e2
  Let ident e1        -> undefined
  IfThenElse p e1 e2  -> undefined

evaluate'UnOp :: UnOp ty -> TExpr ty -> EvalM ty
evaluate'UnOp unOp expr = case unOp of
  Not -> not <$> evaluate' expr
  Abs -> abs <$> evaluate' expr

evaluate'BinOp :: BinOp ty ty' -> TExpr ty -> TExpr ty -> EvalM ty'
evaluate'BinOp binOp expr1 expr2 = case binOp of
  And -> (&&) <$> (evaluate' expr1) <*> (evaluate' expr2)
  Or  -> (||) <$> (evaluate' expr1) <*> (evaluate' expr2)
  Equ -> (==) <$> (evaluate' expr1) <*> (evaluate' expr2)
  Neq -> (/=) <$> (evaluate' expr1) <*> (evaluate' expr2)
  Gt  -> (>)  <$> (evaluate' expr1) <*> (evaluate' expr2)
  Gte -> (>=) <$> (evaluate' expr1) <*> (evaluate' expr2)
  Lt  -> (<)  <$> (evaluate' expr1) <*> (evaluate' expr2)
  Lte -> (<=) <$> (evaluate' expr1) <*> (evaluate' expr2)
  Add -> (+)  <$> (evaluate' expr1) <*> (evaluate' expr2)
  Sub -> (-)  <$> (evaluate' expr1) <*> (evaluate' expr2)
  Mul -> (*)  <$> (evaluate' expr1) <*> (evaluate' expr2)

value :: Value ty -> EvalM ty
value = \case
  VBool b   -> return b
  VInt  i   -> return i
  VDouble d -> return d
