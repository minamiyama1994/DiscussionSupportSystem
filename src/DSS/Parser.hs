module DSS.Parser where

    import Text.Regex
    import Text.Regex.Posix
    import Data.List

    data Discussion = Claim | Consent | Correction | Rebuttal | Supplement | Tree deriving ( Show )
    data Statement = TopStatement String | IndentStatement [ String ] deriving ( Show )
    data PreStatement = PreTop String | PreIndent String

    parse :: String -> [ Statement ]
    parse str = map toStatement $ groupBy isSame $ map lineParse $ filter ( not . null ) $ lines str where
        isSame ( PreIndent _ ) ( PreIndent _ ) = True
        isSame _ _ = False
        toStatement [ PreTop s ] = TopStatement s
        toStatement xs = IndentStatement $ map ( \ ( PreIndent x ) -> x ) xs

    lineParse :: String -> PreStatement
    lineParse str = case str =~ "^\\s*" :: ( String , String , String ) of
        ( _ , "" , body ) -> PreTop body
        ( _ , _ , body ) -> PreIndent body
