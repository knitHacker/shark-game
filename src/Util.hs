{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Util
    ( DataEntry(..)
    , getEntry
    , getEntries
    , getAllEntries
    , getData
    ) where

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile )
import Data.Aeson.Types ( FromJSON, ToJSON )
import Data.Unique ( Unique, hashUnique )
import Control.Monad.IO.Class ()

import qualified Data.Map.Strict as M
import qualified Data.Text as T


instance Show Unique where
    show :: Unique -> String
    show = show . hashUnique

data DataEntry a = Entry
    { entryKey :: T.Text
    , entryData :: a
    } deriving (Generic, Show, Eq)

instance FromJSON a => FromJSON (DataEntry a)
instance ToJSON a => ToJSON (DataEntry a)

getEntry :: M.Map T.Text a -> T.Text -> DataEntry a
getEntry m k = Entry k (m M.! k)

getEntries :: M.Map T.Text [a] -> T.Text -> [DataEntry a]
getEntries m k = Entry k <$> m M.! k

getAllEntries :: M.Map T.Text [a] -> [DataEntry a]
getAllEntries m = concatMap (getEntries m) (M.keys m)

getData :: DataEntry a -> (a -> b) -> b
getData (Entry _ d) f = f d
