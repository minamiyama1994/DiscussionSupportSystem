import System.Environment as Env
import DSS.Parser as P

main :: IO ( )
main = do
    progName <- getProgName
    args <- getArgs
    case args of
        arg : _ -> readFile arg >>= return . P.parse >>= print
        _ -> putStrLn $ "Please command :\n    $ " ++ progName ++ " [SrcFileName]"
