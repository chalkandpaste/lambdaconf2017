{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Typed
  ( checkExpression
  , A(..)
  , (:*:)(..)
  , Type(..)
  , Value(..)
  , UnOp(..)
  , BinOp(..)
  , TExpr(..)
  , TypedExpression
  )
  where

import qualified Untyped as Untyped

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Constraint hiding (Sub)
import Data.Map.Strict as Map

data A f = forall x. A (f x)

data (f :*: g) x = f x :*: g x

data Type ty where
  Bool :: Type Bool
  Int :: Type Int
  Double :: Type Double

data TExpr ty where
  Literal   :: Value ty -> TExpr ty
  Variable  :: String -> Type ty -> TExpr ty -- use of Type here as witness
  UnOp      :: UnOp ty -> TExpr ty -> TExpr ty
  BinOp     :: BinOp ty ty'
            -> TExpr ty
            -> TExpr ty
            -> TExpr ty'
  Let       :: String -> Type ty -> TExpr ty -> TExpr ty' -> TExpr ty'
  IfThenElse :: TExpr Bool
             -> TExpr ty
             -> TExpr ty
             -> TExpr ty

data Value ty where
  VBool   :: Bool   -> Value Bool
  VInt    :: Int    -> Value Int
  VDouble :: Double -> Value Double

data UnOp ty where
  Not :: UnOp Bool
  Abs :: Num ty => UnOp ty

data BinOp ty ty' where
  And :: BinOp Bool Bool
  Or  :: BinOp Bool Bool
  Equ :: Eq ty => BinOp ty Bool
  Neq :: Eq ty => BinOp ty Bool
  Gt  :: Ord ty => BinOp ty Bool
  Gte :: Ord ty => BinOp ty Bool
  Lt  :: Ord ty => BinOp ty Bool
  Lte :: Ord ty => BinOp ty Bool
  Add :: Num ty => BinOp ty ty
  Sub :: Num ty => BinOp ty ty
  Mul :: Num ty => BinOp ty ty

type TypedExpression = TExpr :*: Type

{-                       Context functions                                 -}

type TypeContext = Map String (A Type)

emptyContext :: TypeContext
emptyContext = Map.empty

type TypeCheckM a = StateT TypeContext (Except String) a

lookupContext :: String -> TypeCheckM (Maybe (A Type))
lookupContext ident = Map.lookup ident <$> get

subcontext :: String -> A Type -> TypeCheckM x -> TypeCheckM x
subcontext ident ty x = withStateT (Map.insert ident ty) x
{-                       Checking functions                                 -}

checkExpression :: Untyped.Expression -> Except String (A TypedExpression)
checkExpression expr = evalStateT (checkExpression' expr) emptyContext

checkExpression'
  :: Untyped.Expression
  -> TypeCheckM (A TypedExpression)
checkExpression' = \case
  Untyped.Literal v               -> do
    A (v :*: ty) <- return $ checkValue v
    return . A $ Literal v :*: ty

  Untyped.Variable ident          -> do
    maybeTexpr <- lookupContext ident
    case maybeTexpr of
      Just (A ty) -> return . A $ Variable ident ty :*: ty
      Nothing     -> throwError $ "variable `" ++ ident ++ "` does not exist in scope"

  Untyped.UnOp unOp expr          -> checkUnOp unOp expr
  Untyped.BinOp binOp expr1 expr2 -> checkBinOp binOp expr1 expr2
  Untyped.Let ident expr1 expr2   -> do
    A (ltexpr :*: lty) <- checkExpression' expr1
    A (rtexpr :*: rty) <- subcontext ident (A lty) $ checkExpression' expr2
    return . A $ Let ident lty ltexpr rtexpr :*: rty

  Untyped.IfThenElse pred expr1 expr2 -> do
    A (tpred :*: pTy) <- checkExpression' pred
    A (texpr1 :*: ty1) <- checkExpression' pred
    A (texpr2 :*: ty2) <- checkExpression' pred
    Dict <- checkTypeEquality Bool pTy
    Dict <- checkTypeEquality ty1 ty2

    return . A $ IfThenElse tpred texpr1 texpr2 :*: ty1



checkUnOp
  :: Untyped.UnOp
  -> Untyped.Expression
  -> TypeCheckM (A TypedExpression)
checkUnOp unOp expr =
  case unOp of
    Untyped.Not -> do
      A (te :*: ty) <- checkExpression' expr
      case ty of
        Bool -> return . A $ UnOp Not te :*: ty
        _    -> throwError "expected bool but got something way different"
    Untyped.Abs -> do
      A (te :*: ty) <- checkExpression' expr
      Dict <- checkNum ty
      return . A $ UnOp Abs te :*: ty

checkBinOp
  :: Untyped.BinOp
  -> Untyped.Expression
  -> Untyped.Expression
  -> TypeCheckM (A TypedExpression)
checkBinOp binOp expr1 expr2 =
  case binOp of
    Untyped.And -> do
      A (te1 :*: ty1) <- checkExpression' expr1
      A (te2 :*: ty2) <- checkExpression' expr2
      case (ty1, ty2) of
        (Bool, Bool) -> return . A $ BinOp And te1 te2 :*: Bool
        _            -> throwError "&&: left and right hand sides are not of same type"
    Untyped.Or  -> do
      A (te1 :*: ty1) <- checkExpression' expr1
      A (te2 :*: ty2) <- checkExpression' expr2
      case (ty1, ty2) of
        (Bool, Bool) -> return . A $ BinOp Or te1 te2 :*: Bool
        (_,_)        -> throwError "||: left and right hand sides are not of same type"
    Untyped.Equ -> do
      A (te1 :*: ty1) <- checkExpression' expr1
      A (te2 :*: ty2) <- checkExpression' expr2
      Dict <- checkTypeEquality ty1 ty2
      Dict <- checkEq ty1
      return . A $ BinOp Equ te1 te2 :*: Bool
    Untyped.Neq -> do
      A (te1 :*: ty1) <- checkExpression' expr1
      A (te2 :*: ty2) <- checkExpression' expr2
      Dict <- checkTypeEquality ty1 ty2
      Dict <- checkEq ty1
      return . A $ BinOp Neq te1 te2 :*: Bool
    Untyped.Gt  -> do
      A (te1 :*: ty1) <- checkExpression' expr1
      A (te2 :*: ty2) <- checkExpression' expr2
      Dict <- checkTypeEquality ty1 ty2
      Dict <- checkOrd ty1
      return . A $ BinOp Gt te1 te2 :*: Bool
    Untyped.Gte -> do
      A (te1 :*: ty1) <- checkExpression' expr1
      A (te2 :*: ty2) <- checkExpression' expr2
      Dict <- checkTypeEquality ty1 ty2
      Dict <- checkOrd ty1
      return . A $ BinOp Gte te1 te2 :*: Bool
    Untyped.Lt  -> do
      A (te1 :*: ty1) <- checkExpression' expr1
      A (te2 :*: ty2) <- checkExpression' expr2
      Dict <- checkTypeEquality ty1 ty2
      Dict <- checkOrd ty1
      return . A $ BinOp Lt te1 te2 :*: Bool
    Untyped.Lte -> do
      A (te1 :*: ty1) <- checkExpression' expr1
      A (te2 :*: ty2) <- checkExpression' expr2
      Dict <- checkTypeEquality ty1 ty2
      Dict <- checkOrd ty1
      return . A $ BinOp Lte te1 te2 :*: Bool
    Untyped.Add -> do
      A (te1 :*: ty1) <- checkExpression' expr1
      A (te2 :*: ty2) <- checkExpression' expr2
      Dict <- checkTypeEquality ty1 ty2
      Dict <- checkNum ty1
      return . A $ BinOp Add te1 te2 :*: ty1
    Untyped.Sub -> do
      A (te1 :*: ty1) <- checkExpression' expr1
      A (te2 :*: ty2) <- checkExpression' expr2
      Dict <- checkTypeEquality ty1 ty2
      Dict <- checkNum ty1
      return . A $ BinOp Sub te1 te2 :*: ty1
    Untyped.Mul -> do
      A (te1 :*: ty1) <- checkExpression' expr1
      A (te2 :*: ty2) <- checkExpression' expr2
      Dict <- checkTypeEquality ty1 ty2
      Dict <- checkNum ty1
      return . A $ BinOp Mul te1 te2 :*: ty1

checkTypeEquality
  :: Type ty
  -> Type ty'
  -> TypeCheckM (Dict (ty ~ ty'))
checkTypeEquality ty1 ty2 =
  case (ty1, ty2) of
    (Bool, Bool)     -> return Dict
    (Int, Int)       -> return Dict
    (Double, Double) -> return Dict
    _                -> throwError "left and right hand types do not match"

checkNum :: Type ty -> TypeCheckM (Dict (Num ty))
checkNum Bool   = throwError "Bool does not satisfy Num constraint"
checkNum Int    = return Dict
checkNum Double = return Dict

checkOrd :: Type ty -> TypeCheckM (Dict (Ord ty))
checkOrd Bool   = return Dict
checkOrd Int    = return Dict
checkOrd Double = return Dict

checkEq :: Type ty -> TypeCheckM (Dict (Eq ty))
checkEq Bool   = return Dict
checkEq Int    = return Dict
checkEq Double = return Dict


checkValue :: Untyped.Value -> A (Value :*: Type)
checkValue = \case
  Untyped.VBool b   -> A $ VBool b   :*: Bool
  Untyped.VInt i    -> A $ VInt i    :*: Int
  Untyped.VDouble d -> A $ VDouble d :*: Double

