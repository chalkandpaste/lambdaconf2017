{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval (evaluate) where

import Typed hiding (lookupContext, subcontext)

import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Map.Strict as Map
import Data.Maybe


type EvalContext = Map String (A Value)

emptyContext :: EvalContext
emptyContext = Map.empty

type EvalM a = State EvalContext a

lookupContext :: String -> EvalM (A Value)
lookupContext ident = fromJust . Map.lookup ident <$> get

subcontext :: String -> A Value -> EvalM x -> EvalM x
subcontext ident ty x = withStateT (Map.insert ident ty) x

evaluate :: TExpr ty -> ty
evaluate texpr = evalState (evaluate' texpr) emptyContext

evaluate' :: forall ty. TExpr ty -> EvalM ty
evaluate' = \case
  Literal v           -> value v
  Variable ident ty   -> do
    A tval <- lookupContext ident
    -- unfortunately we have to witness to the type here
    case (tval, ty) of
      (VBool b, Bool)     -> return b
      (VInt i, Int)       -> return i
      (VDouble d, Double) -> return d
  UnOp unOp expr      -> evaluateUnOp unOp expr
  BinOp binOp e1 e2   -> evaluateBinOp binOp e1 e2
  Let ident (ty :: Type t) (e1 :: TExpr t) e2  -> do
    (v :: t) <- evaluate' e1
    let
      val = case ty of
        Bool   -> A $ VBool v
        Int    -> A $ VInt v
        Double -> A $ VDouble v
    subcontext ident val $ evaluate' e2

  IfThenElse p e1 e2  -> do
    pv <- evaluate' p
    if pv then evaluate' e1 else evaluate' e2

evaluateUnOp :: UnOp ty -> TExpr ty -> EvalM ty
evaluateUnOp unOp expr = case unOp of
  Not -> not <$> evaluate' expr
  Abs -> abs <$> evaluate' expr

evaluateBinOp :: BinOp ty ty' -> TExpr ty -> TExpr ty -> EvalM ty'
evaluateBinOp binOp expr1 expr2 = case binOp of
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
