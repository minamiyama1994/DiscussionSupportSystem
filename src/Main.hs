{-# OPTIONS -Wall #-}
import System.Environment
import DSS.Parser as DP

main :: IO ( )
main = do
    args <- getArgs
    case args of
        [ ] -> do
            progName <- getProgName
            putStrLn $ "please command"
            putStrLn $ "    $ " ++ progName ++ " [FILENAME]"
            putStrLn $ "\nprogram " ++ progName ++ " use environment variable"
            putStrLn $ "    DSS_IMPORT_PATH"
        fileName : _ -> do
            res <- readFile fileName >>= return . DP.parse
            print res