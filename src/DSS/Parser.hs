module DSS.Parser where

    import Text.Regex
    import Text.Regex.Posix
    import Data.List

    data Discussion = Claim | Consent | Correction | Rebuttal | Supplement | Tree deriving ( Show )
    data Statement = TopStatement String | IndentStatement [ String ] deriving ( Show )

    parse :: String -> [ Statement ]
    parse str = map toStatement $ groupBy isSame $ map lineParse $ filter ( not . null ) $ lines str where
        isSame ( Right _ ) ( Right _ ) = True
        isSame _ _ = False
        toStatement [ Left s ] = TopStatement s
        toStatement xs = IndentStatement $ map ( \ ( Right x ) -> x ) xs

    lineParse :: String -> Either String String
    lineParse str = case str =~ "^\\s*" :: ( String , String , String ) of
        ( _ , "" , body ) -> Left body
        ( _ , _ , body ) -> Right body
