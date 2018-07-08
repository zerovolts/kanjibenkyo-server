{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant.API
import Servant.Server
import Network.Wai.Handler.Warp
import Control.Monad.Trans.Reader
import GHC.Generics (Generic)
import GHC.TypeLits
import Data.Aeson
import Database.Beam
import Database.Beam.Postgres
import Data.Proxy
import Data.Text (Text)
import Data.Vector

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

data KanjiTable f = Kanji
  { _kanjiId :: Columnar f Int
  , _kanjiCharacter :: Columnar f Text
  , _kanjiRadical :: Columnar f Text
  , _kanjiKunyomi :: Columnar f (Vector Text)
  , _kanjiOnyomi :: Columnar f (Vector Text)
  , _kanjiMeaning :: Columnar f (Vector Text)
  , _kanjiStrokes :: Columnar f Int
  , _kanjiJlpt :: Columnar (Nullable f) Int
  , _kanjiGrade :: Columnar (Nullable f) Int
  } deriving stock (Generic)
    deriving anyclass (Beamable)

instance ToJSON (KanjiTable Identity) -- fill this in

instance Table KanjiTable where
  data PrimaryKey KanjiTable f = KanjiId (Columnar f Int)
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey :: KanjiTable column -> PrimaryKey KanjiTable column