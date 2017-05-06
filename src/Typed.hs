{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Typed where

import qualified Untyped as Untyped

import Control.Monad.Except
import Data.Constraint hiding (Sub)
import Data.Type.Equality

data f ::: g = forall x. f x ::: g x

data Type ty where
  Bool :: Type Bool
  Int :: Type Int
  Double :: Type Double

instance TestEquality Type where
  testEquality Bool Bool      = Just Refl
  testEquality Int  Int       = Just Refl
  testEquality Double Double  = Just Refl
  testEquality _    _         = Nothing

data TypedExpression ty where
  Literal   :: Value ty -> TypedExpression ty
  UnOp      :: UnOp ty -> TypedExpression ty -> TypedExpression ty
  BinOp     :: BinOp ty ty'
            -> TypedExpression ty
            -> TypedExpression ty
            -> TypedExpression ty'

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

checkExpression
  :: Untyped.SimpleExpression
  -> Except String (TypedExpression ::: Type)
checkExpression = \case
  Untyped.SLiteral v               -> do
    v ::: ty <- return $ checkValue v
    return $ Literal v ::: ty

  Untyped.SUnOp unOp expr          -> checkUnOp unOp expr
  Untyped.SBinOp binOp expr1 expr2 -> checkBinOp binOp expr1 expr2

checkNum :: Type ty -> Maybe (Dict (Num ty))
checkNum Bool   = Nothing
checkNum Int    = Just Dict
checkNum Double = Just Dict

checkOrd :: Type ty -> Maybe (Dict (Ord ty))
checkOrd Bool   = Just Dict
checkOrd Int    = Just Dict
checkOrd Double = Just Dict

checkEq :: Type ty -> Maybe (Dict (Eq ty))
checkEq Bool   = Just Dict
checkEq Int    = Just Dict
checkEq Double = Just Dict

checkUnOp
  :: Untyped.UnOp
  -> Untyped.SimpleExpression
  -> Except String (TypedExpression ::: Type)
checkUnOp unOp expr =
  case unOp of
    Untyped.Not -> do
      te ::: ty <- checkExpression expr
      case testEquality ty Bool of
        Nothing -> throwError "expected bool but got something way different"
        Just Refl -> return $ UnOp Not te ::: ty
    Untyped.Abs -> do
      te ::: ty <- checkExpression expr
      case checkNum ty of
        Nothing -> throwError "expected num but got something way different"
        Just Dict -> return $ UnOp Abs te ::: ty

checkBinOp
  :: Untyped.BinOp
  -> Untyped.SimpleExpression
  -> Untyped.SimpleExpression
  -> Except String (TypedExpression ::: Type)
checkBinOp binOp expr1 expr2 =
  case binOp of
    Untyped.And -> do
      te1 ::: ty1 <- checkExpression expr1
      te2 ::: ty2 <- checkExpression expr2
      case (testEquality ty1 Bool, testEquality ty2 Bool) of
        (_,_) -> throwError "left and right hand sides are not of same type"
        (Just Refl, Just Refl) -> return $ BinOp And te1 te2 ::: Bool
    Untyped.Or  -> do
      te1 ::: ty1 <- checkExpression expr1
      te2 ::: ty2 <- checkExpression expr2
      case (testEquality ty1 Bool, testEquality ty2 Bool) of
        (_,_) -> throwError "left and right hand sides are not of same type"
        (Just Refl, Just Refl) -> return $ BinOp Or te1 te2 ::: Bool
    Untyped.Equ -> do
      te1 ::: ty1 <- checkExpression expr1
      te2 ::: ty2 <- checkExpression expr2
      case (testEquality ty1 ty2, checkEq ty1, checkEq ty2) of
        (Nothing, _, _) -> throwError "left and right hand sides are not of same type"
        (Just Refl, Nothing, Nothing) -> throwError "types must have equality"
        (Just Refl, Just Dict, Just Dict) -> return $ BinOp Equ te1 te2 ::: Bool
        _ -> throwError "should not be here"
    Untyped.Neq -> do
      te1 ::: ty1 <- checkExpression expr1
      te2 ::: ty2 <- checkExpression expr2
      case (testEquality ty1 ty2, checkEq ty1, checkEq ty2) of
        (Nothing, _, _) -> throwError "left and right hand sides are not of same type"
        (Just Refl, Nothing, Nothing) -> throwError "types must have equality"
        (Just Refl, Just Dict, Just Dict) -> return $ BinOp Neq te1 te2 ::: Bool
        _ -> throwError "should not be here"
    Untyped.Gt  -> do
      te1 ::: ty1 <- checkExpression expr1
      te2 ::: ty2 <- checkExpression expr2
      case (testEquality ty1 ty2, checkOrd ty1, checkOrd ty2) of
        (Nothing, _, _) -> throwError "left and right hand sides are not of same type"
        (Just Refl, Nothing, Nothing) -> throwError "types must have a comparator"
        (Just Refl, Just Dict, Just Dict) -> return $ BinOp Gt te1 te2 ::: Bool
        _ -> throwError "should not be here"
    Untyped.Gte -> do
      te1 ::: ty1 <- checkExpression expr1
      te2 ::: ty2 <- checkExpression expr2
      case (testEquality ty1 ty2, checkOrd ty1, checkOrd ty2) of
        (Nothing, _, _) -> throwError "left and right hand sides are not of same type"
        (Just Refl, Nothing, Nothing) -> throwError "types must have a comparator"
        (Just Refl, Just Dict, Just Dict) -> return $ BinOp Gte te1 te2 ::: Bool
        _ -> throwError "should not be here"
    Untyped.Lt  -> do
      te1 ::: ty1 <- checkExpression expr1
      te2 ::: ty2 <- checkExpression expr2
      case (testEquality ty1 ty2, checkOrd ty1, checkOrd ty2) of
        (Nothing, _, _) -> throwError "left and right hand sides are not of same type"
        (Just Refl, Nothing, Nothing) -> throwError "types must have a comparator"
        (Just Refl, Just Dict, Just Dict) -> return $ BinOp Lt te1 te2 ::: Bool
        _ -> throwError "should not be here"
    Untyped.Lte -> do
      te1 ::: ty1 <- checkExpression expr1
      te2 ::: ty2 <- checkExpression expr2
      case (testEquality ty1 ty2, checkOrd ty1, checkOrd ty2) of
        (Nothing, _, _) -> throwError "left and right hand sides are not of same type"
        (Just Refl, Nothing, Nothing) -> throwError "types must have a comparator"
        (Just Refl, Just Dict, Just Dict) -> return $ BinOp Lte te1 te2 ::: Bool
        _ -> throwError "should not be here"
    Untyped.Add -> do
      te1 ::: ty1 <- checkExpression expr1
      te2 ::: ty2 <- checkExpression expr2
      {-checkConstraint Add (checkNum) te1 ty1 te2 ty2 ty1-}
      case (testEquality ty1 ty2, checkNum ty1, checkNum ty2) of
        (Nothing, _, _) -> throwError "left and right hand sides are not of same type"
        (Just Refl, Nothing, Nothing) -> throwError "types must be nums"
        (Just Refl, Just Dict, Just Dict) -> return $ BinOp Add te1 te2 ::: ty1
        _ -> throwError "should not be here"
    Untyped.Sub -> do
      te1 ::: ty1 <- checkExpression expr1
      te2 ::: ty2 <- checkExpression expr2
      {-checkConstraint Add (checkNum) te1 ty1 te2 ty2 ty1-}
      case (testEquality ty1 ty2, checkNum ty1, checkNum ty2) of
        (Nothing, _, _) -> throwError "left and right hand sides are not of same type"
        (Just Refl, Nothing, Nothing) -> throwError "types must be nums"
        (Just Refl, Just Dict, Just Dict) -> return $ BinOp Sub te1 te2 ::: ty1
        _ -> throwError "should not be here"
    Untyped.Mul -> do
      te1 ::: ty1 <- checkExpression expr1
      te2 ::: ty2 <- checkExpression expr2
      {-checkConstraint Add (checkNum) te1 ty1 te2 ty2 ty1-}
      case (testEquality ty1 ty2, checkNum ty1, checkNum ty2) of
        (Nothing, _, _) -> throwError "left and right hand sides are not of same type"
        (Just Refl, Nothing, Nothing) -> throwError "types must be nums"
        (Just Refl, Just Dict, Just Dict) -> return $ BinOp Mul te1 te2 ::: ty1
        _ -> throwError "should not be here"
  where
    checkConstraint
      :: BinOp ty ty'
      -> (Type ty -> Maybe (Dict (c ty)))
      -> TypedExpression ty
      -> Type ty
      -> TypedExpression ty
      -> Type ty
      -> Type ty'
      -> Except String (TypedExpression ::: Type)
    checkConstraint binOp checker te1 ty1 te2 ty2 rty =
      case (testEquality ty1 ty2, checkNum ty1, checkNum ty2) of
        (Nothing, _, _) -> throwError "left and right hand sides are not of same type"
        (Just Refl, Nothing, Nothing) -> throwError "types must be nums"
        (Just Refl, Just Dict, Just Dict) -> return $ BinOp binOp te1 te2 ::: rty
        _ -> throwError "should not be here"


checkValue :: Untyped.Value -> Value ::: Type
checkValue = \case
  Untyped.VBool b   -> VBool b   ::: Bool
  Untyped.VInt i    -> VInt i    ::: Int
  Untyped.VDouble d -> VDouble d ::: Double

value :: Value ty -> ty
value = \case
  VBool b   -> b
  VInt  i   -> i
  VDouble d -> d

evaluate :: TypedExpression ty -> ty
evaluate = \case
  Literal v         -> value v
  UnOp unOp expr    -> evaluateUnOp unOp expr
  BinOp binOp e1 e2 -> evaluateBinOp binOp e1 e2

evaluateUnOp :: UnOp ty -> TypedExpression ty -> ty
evaluateUnOp unOp expr =
  case unOp of
    Not -> not $ evaluate expr
    Abs -> abs $ evaluate expr

evaluateBinOp :: BinOp ty ty' -> TypedExpression ty -> TypedExpression ty -> ty'
evaluateBinOp binOp expr1 expr2 = case binOp of
    And -> (evaluate expr1) && (evaluate expr2)
    Or -> (evaluate expr1) || (evaluate expr2)
    Equ -> (evaluate expr1) == (evaluate expr2)
    Neq -> (evaluate expr1) /= (evaluate expr2)
    Gt -> (evaluate expr1) > (evaluate expr2)
    Gte -> (evaluate expr1) >= (evaluate expr2)
    Lt -> (evaluate expr1) < (evaluate expr2)
    Lte -> (evaluate expr1) <= (evaluate expr2)
    Add -> (evaluate expr1) + (evaluate expr2)
    Sub -> (evaluate expr1) - (evaluate expr2)
    Mul -> (evaluate expr1) * (evaluate expr2)

