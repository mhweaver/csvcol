module Lib
    ( intercalate'
    , unlines'
    , pad
    ) where

import Data.Int  (Int64)
import qualified Data.ByteString.Lazy.Char8 as L

--
-- Intercalate a ByteString with a character given as a String
intercalate' :: String -> [L.ByteString] -> L.ByteString
intercalate' = L.intercalate . L.pack

-- Like unlines from Prelude, but for ByteString
unlines' :: [L.ByteString] -> L.ByteString
unlines' = intercalate' "\n"

pad :: Char -> Int64 -> L.ByteString -> L.ByteString
pad c maxLength str = str `L.append` L.replicate (maxLength - L.length str) c 
