{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Repl
  ( repl
  , runInputT
  , defaultSettings
  ) where

import Parser
import Typed
import Untyped


import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Trans.Except

import Data.Map.Lazy as Map
import Data.Constraint

import System.Console.Haskeline
import Text.Megaparsec

repl :: InputT IO ()
repl = do
  inputMaybe <- getInputLine "> "
  case inputMaybe of
    Nothing -> return () -- Ctrl + D
    Just input -> doExcept $ do
      parsed <- withExcept show . except $ parse expressionParser "" input
      simplified <- simplifyExpression Map.empty parsed
      typed ::: ty <- checkExpression simplified
      case checkShow ty of
        Just Dict -> return . show $ evaluate typed
        Nothing -> throwError "this ought not happen"
  where
    checkShow :: Type x -> Maybe (Dict (Show x))
    checkShow Int    = Just Dict
    checkShow Bool   = Just Dict
    checkShow Double = Just Dict
    checkShow _      = Nothing

    cont :: Show a => a -> InputT IO ()
    cont x = lift (print x) >> repl

    doEither :: Show a => Either String a -> InputT IO ()
    doEither = either cont cont

    doExcept :: Show a => Except String a -> InputT IO ()
    doExcept = doEither . runExcept
