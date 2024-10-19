module Main (main) where

import Text.ParserCombinators.Parsec ( parse )
import qualified Lib.HTML as HTML

main :: IO ()
main =
    do c <- getContents
       case parse HTML.html "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r

