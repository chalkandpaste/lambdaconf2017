{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Repl
  ( repl
  , runInputT
  , defaultSettings
  ) where

import Eval
import Parser
import Typed
import Untyped

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Except

import Data.Constraint
import Data.Map.Lazy as Map

import System.Console.Haskeline
import Text.Megaparsec

repl :: InputT IO ()
repl = do
  inputMaybe <- getInputLine "> "
  case inputMaybe of
    Nothing -> return () -- Ctrl + D

    Just input -> doExcept $ do
      expr <- withExcept show . except $ parse expressionParser "" input
      A (texpr :*: ty) <- checkExpression expr
      Dict <- checkShow ty
      return . show $ evaluate texpr

cont :: Show a => a -> InputT IO ()
cont x = lift (print x) >> repl

doEither :: Show a => Either String a -> InputT IO ()
doEither = either cont cont

doExcept :: Show a => Except String a -> InputT IO ()
doExcept = doEither . runExcept

checkShow :: Monad m => Type x -> m (Dict (Show x))
checkShow Int    = return Dict
checkShow Bool   = return Dict
checkShow Double = return Dict

