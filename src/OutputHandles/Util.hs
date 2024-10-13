module OutputHandles.Util
    (
    ) where

import qualified Data.Text as T

import OutputHandles.Types

wrapText :: OutputHandles -> T.Text -> Int -> Int -> Int -> Int -> Int -> Color -> [TextDisplay]
wrapText outs txt x yStart width lineHeight fontSize c = undefined