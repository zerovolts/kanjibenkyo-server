{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module KanjiBenkyo.Tables.KanjiTable (KanjiTable) where

import Data.Aeson
import Data.Text (Text)
import Data.Vector
import Database.Beam
import GHC.Generics (Generic)

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
  primaryKey x = KanjiId (_kanjiId x)