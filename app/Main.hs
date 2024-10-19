module Main (main) where

import qualified Lib.CSV as CSV
import qualified Lib.HTML as HTML
import Text.ParserCombinators.Parsec ( parse )

main :: IO ()
main =
    do c <- getContents
       case parse CSV.csvFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r

