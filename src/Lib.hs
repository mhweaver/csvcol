module Lib
    ( intercalate'
    , unlines'
    , pad
    ) where

import Data.Int  (Int64)
import qualified Data.ListLike as LL
import qualified Data.ByteString.Lazy.Char8 as L

--
-- Intercalate a ByteString with a character given as a String
intercalate' :: String -> [L.ByteString] -> L.ByteString
intercalate' = L.intercalate . L.pack

-- Like unlines from Prelude, but for ByteString
unlines' :: [L.ByteString] -> L.ByteString
unlines' = intercalate' "\n"

pad :: (LL.ListLike l c, Integral i) => c -> i -> l -> l
pad c maxLength cs = cs `LL.append` LL.replicate (fromIntegral maxLength - LL.length cs) c 
