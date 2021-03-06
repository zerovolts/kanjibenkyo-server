{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant.API
import Servant.Server
import Network.Wai.Handler.Warp
import Control.Monad.Trans.Reader
import GHC.Generics (Generic)
import GHC.TypeLits
import Database.Beam
import Database.Beam.Postgres
import Data.Proxy

import KanjiBenkyo.Tables.KanjiTable

type API = SimpleAPI "kanji" (KanjiTable Identity) Int

type SimpleAPI (name :: Symbol) a i = name :>
  ( Get '[JSON] [a]
  :<|> Capture "id" i :> Get '[JSON] a
  )

server :: Connection -> ServerT API Handler
server conn = hoistServer
  (Proxy :: Proxy API)
  (\r -> runReaderT r conn)
  handlers

handlers :: ServerT API (ReaderT Connection Handler)
handlers = kanjiHandler

simpleHandler
  :: (ReaderT Connection Handler [a])
  -> (i -> ReaderT Connection Handler a)
  -> ServerT (SimpleAPI name a i) (ReaderT Connection Handler)
simpleHandler listAs getA =
  listAs :<|> getA

kanjiHandler :: ServerT (SimpleAPI "kanji" (KanjiTable Identity) Int) (ReaderT Connection Handler)
kanjiHandler = simpleHandler
  getAllKanji
  undefined
  -- (\i -> pure (kanjiList !! i))

getAllKanji :: ReaderT Connection Handler [KanjiTable Identity]
getAllKanji = do
  conn <- ask
  liftIO $ runBeamPostgresDebug putStrLn conn $ runSelectReturningList $ select $ all_ (dbKanjiTable myDatabase)

startServer :: Connection -> Port -> IO ()
startServer conn port = run port (serve (Proxy :: Proxy API) (server conn))

main :: IO ()
main = do
  conn <- connectPostgreSQL "host='localhost' port='5432' dbname='japanese_development'"
  startServer conn 9999

data MyDatabase f = MyDatabase
  { dbKanjiTable :: f (TableEntity KanjiTable)
  } deriving stock (Generic) 
    deriving anyclass (Database be)

myDatabase :: MyDatabase (DatabaseEntity be MyDatabase)
myDatabase = defaultDbSettings `withDbModification`
  dbModification {
    dbKanjiTable = modifyTable (\_ -> "kanji") tableModification
  }
