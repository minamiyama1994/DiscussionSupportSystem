{-# OPTIONS -Wall #-}
import System.Environment
import DSS.Parser as DP

main :: IO ( )
main = do
    file_name : _ <- getArgs
    res <- readFile file_name >>= ( return . DP.parse )
    print res