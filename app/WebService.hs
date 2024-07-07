{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Text.Blaze.Html

import Parser (
  lexer,
  pyhs,
  Exp(..)
  )

import ToMon (
  toMon
  )

type API = "api" :> "expression" :> "parser" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] ParseResponse

data ExprInfo = ExprInfo {
  expr :: String
  } deriving (Generic, Show)

data ParseResponse = ParseResponse {
  parseExp :: String 
  } deriving (Generic, Show)

instance FromJSON ExprInfo
instance ToJSON ParseResponse

parserClient :: ExprInfo -> ParseResponse
parserClient exp =
  ParseResponse  (show exp') where
  exp' = pyhs (lexer (expr exp))

compilerService :: Server API
compilerService = parsePOST
  where
    parsePOST :: ExprInfo -> Servant.Handler ParseResponse
    parsePOST exp = return (parserClient exp)

compilerAPI :: Proxy API
compilerAPI = Proxy

service1 :: Application
service1 = serve compilerAPI compilerService

main :: IO ()
main = run 8081 service1
