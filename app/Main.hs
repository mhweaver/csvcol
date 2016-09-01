module Main where

import Lib
import Data.Csv
import Data.List (intercalate
                 , transpose
                 , maximum)
import Data.Int  (Int64)

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as L

type Row = [L.ByteString]

main :: IO ()
main = do
    rawCsv <- L.getContents
    let csv       = V.toList . fmap V.toList . parseCsv $ rawCsv :: [Row]
        colWidths = columnWidths csv
        tabbedCsv = unlines' . map (rowToColumnizedString colWidths) $ csv
    L.putStrLn tabbedCsv
    return ()

-- Read the raw CSV data and return structured data
parseCsv :: L.ByteString -> V.Vector (V.Vector L.ByteString)
parseCsv raw = case (decode NoHeader raw) of
              (Right out) -> out
              (Left err)  -> error err

-- Take a list of maximum column lengths and a list of strings for each column, pad the strings out to the
-- maximum length for their respective column
rowToColumnizedString :: [Int64] -> Row -> L.ByteString
rowToColumnizedString colWidths row = intercalate' "  " $ zipWith (pad ' ') colWidths row

-- Given a list of tabular data, determine that maximum width of each column
columnWidths :: [Row] -> [Int64]
columnWidths rows = colWidths where
                      colWidths = fmap maxLength columns :: [Int64]
                      maxLength = maximum         
                                . fmap L.length
                      columns   = transpose rows
