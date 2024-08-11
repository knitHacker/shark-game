module Utils
    ( randomValue
    ) where


import Control.Monad.IO.Class ( MonadIO )
import System.Random ( Random, randomRIO )


randomValue :: (Random a, MonadIO m) => a -> a -> m a
randomValue start end = randomRIO (start, end)
