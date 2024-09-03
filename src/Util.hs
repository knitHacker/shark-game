module Util
    ( DataEntry(..)
    , getEntry
    , getData
    , entryData
    , entryKey
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

data DataEntry a = Entry T.Text a

getEntry :: M.Map T.Text a -> T.Text -> DataEntry a
getEntry m k = Entry k (m M.! k)

getData :: DataEntry a -> (a -> b) -> b
getData (Entry _ d) f = f d

entryData :: DataEntry a -> a
entryData (Entry _ d) = d

entryKey :: DataEntry a -> T.Text
entryKey (Entry k _) = k
