{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Util
    ( DataEntry(..)
    , getEntry
    , getData
    ) where

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile )
import Data.Aeson.Types ( FromJSON, ToJSON )

import qualified Data.Map.Strict as M
import qualified Data.Text as T

data DataEntry a = Entry
    { entryKey :: T.Text
    , entryData :: a
    } deriving (Generic, Show, Eq)

instance FromJSON a => FromJSON (DataEntry a)
instance ToJSON a => ToJSON (DataEntry a)

getEntry :: M.Map T.Text a -> T.Text -> DataEntry a
getEntry m k = Entry k (m M.! k)

getData :: DataEntry a -> (a -> b) -> b
getData (Entry _ d) f = f d
