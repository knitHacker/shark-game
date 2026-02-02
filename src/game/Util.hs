{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Util
    ( DataEntry(..)
    , DataEntryT
    , getEntry
    , getEntries
    , getAllEntries
    , getData
    , getNestedEntry
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

data DataEntry a b = Entry
    { entryKey :: a
    , entryData :: b
    } deriving (Generic, Show, Eq)

instance (FromJSON a, FromJSON b) => FromJSON (DataEntry a b)
instance (ToJSON a, ToJSON b) => ToJSON (DataEntry a b)

type DataEntryT b = DataEntry T.Text b

getEntry :: M.Map T.Text a -> T.Text -> DataEntryT a
getEntry m k = Entry k (m M.! k)

getEntries :: M.Map T.Text [a] -> T.Text -> [DataEntryT a]
getEntries m k = Entry k <$> m M.! k

getAllEntries :: M.Map T.Text [a] -> [DataEntryT a]
getAllEntries m = concatMap (getEntries m) (M.keys m)

getData :: DataEntry a b -> (b -> c) -> c
getData (Entry _ d) f = f d

getNestedEntry :: M.Map T.Text a -> (a -> M.Map T.Text b) -> (a -> b -> c) -> T.Text -> T.Text -> DataEntry (T.Text, T.Text) c
getNestedEntry topM getNestedM combine k1 k2 =
    let topData = topM M.! k1
        nestedM = getNestedM topData
        nestedData = nestedM M.! k2
    in Entry (k1, k2) (combine topData nestedData)
