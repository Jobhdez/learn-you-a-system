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

import ToSelect (
  toSelect
  )

type API = "api" :> "expression" :> "ast" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] ParseResponse
      :<|> "api" :> "expression" :> "monadic" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] MonadicResponse
      :<|> "api" :> "expression" :> "selection" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] SelectInstructionResponse

data ExprInfo = ExprInfo {
  expr :: String
  } deriving (Generic, Show)

data ParseResponse = ParseResponse {
  parseExp :: String 
  } deriving (Generic, Show)

data MonadicResponse = MonadicResponse {
  monExp :: String 
  } deriving (Generic, Show)


data SelectInstructionResponse = SelectInstructionResponse {
  selectExp :: String 
  } deriving (Generic, Show)

data AstRecord = AstRecord {
  input :: Value,
  ast :: Value
  }
instance FromJSON ExprInfo
instance ToJSON MonadicResponse
instance ToJSON ParseResponse
instance ToJSON SelectInstructionResponse

instance FromRow AstRecord where
  fromRow = AstRecord <$> field <*> field

connectionInfo :: ConnectInfo
connectionInfo =
  defaultConnectInfo { connectHost = "127.0.0.1",
                       connectPort = 5432,
                       connectUser = "pyhs",
                       connectPassword = "hello123",
                       connectDatabase= "compiler_web"
                     }
initDB :: ConnectInfo -> IO ()
initDB conn = bracket (connect conn) close $ \con -> do

    let createTable1 = "CREATE TABLE IF NOT EXISTS select_e (id SERIAL PRIMARY KEY, input JSONB, ast JSONB, mon JSONB, select_exp JSONB)"
    let createTable2 = "CREATE TABLE IF NOT EXISTS ast_e (id SERIAL PRIMARY KEY, input JSONB, ast JSONB)"
    let createTable3 = "CREATE TABLE IF NOT EXISTS mon_e (id SERIAL PRIMARY KEY, input JSONB, ast JSONB, mon JSONB)"
    
    _ <- execute_ con createTable1
    _ <- execute_ con createTable2
    _ <- execute_ con createTable3

    return ()

myPool :: IO (Pool Connection)
myPool = createPool (connect connectionInfo) close 1 10 10

  
parserClient :: ExprInfo -> ParseResponse
parserClient exp =
  ParseResponse  (show exp') where
  exp' = pyhs (lexer (expr exp))

monadicClient :: ExprInfo -> MonadicResponse
monadicClient exp =
  MonadicResponse (show exp') where
  exp' = toMon ast 0 where
    ast = pyhs (lexer (expr exp))

selectInstructionClient :: ExprInfo -> SelectInstructionResponse
selectInstructionClient exp =
  SelectInstructionResponse (show exp')
  where
    exp' = toSelect mon
      where
        mon = toMon ast 0
          where
            ast = pyhs (lexer (expr exp))

compilerService :: Pool Connection -> Server API
compilerService pool =
  parsePOST
  :<|> monadicPOST
  :<|> selectPOST
  where
    parsePOST :: ExprInfo -> Servant.Handler ParseResponse
    parsePOST exp = do
      let ast = pyhs (lexer (expr exp))
      liftIO $ withResource pool $ \conn ->
        execute conn "INSERT INTO ast_e (input, ast) VALUES (?, ?)" (toJSON (show (expr exp)), toJSON (show ast))
      return (parserClient exp)


    monadicPOST :: ExprInfo -> Servant.Handler MonadicResponse
    monadicPOST exp = do
      let ast = pyhs (lexer (expr exp))
      let mon = toMon ast 0
      liftIO $ withResource pool $ \conn ->
        execute conn "INSERT INTO mon_e (input, ast, mon) VALUES (?, ?, ?)" (toJSON (show (expr exp)), toJSON (show ast), toJSON (show mon))
      return (monadicClient exp)

    selectPOST :: ExprInfo -> Servant.Handler SelectInstructionResponse
    selectPOST exp = do
      let ast = pyhs (lexer (expr exp))
      let mon = toMon ast 0
      let ss = toSelect mon
      liftIO $ withResource pool $ \conn ->
        execute conn "INSERT INTO select_e (input, ast, mon, select_exp) VALUES (?,?,?,?)" (toJSON (show (expr exp)), toJSON (show ast), toJSON (show mon), toJSON (show ss))
      return (selectInstructionClient exp)
    
compilerAPI :: Proxy API
compilerAPI = Proxy

service1 :: Pool Connection -> Application
service1 pool = serve compilerAPI (compilerService pool)

main :: IO ()
main = do
  pool <- myPool
  initDB connectionInfo
  run 8081 (service1 pool)
