module DSS.Parser where

    import Data.Char
    import Text.ParserCombinators.ReadP

    data Discussion = Claim | Consent | Correction | Rebuttal | Supplement deriving ( Show )
    data Expression = Nand Expression Expression | Primary String deriving ( Show )

    parse :: ReadS Discussion
    parse = \ s -> ( readP_to_S $ choice $ [ parseClaim , parseConsent , parseRebuttal , parseSupplement ] ) s

    parseClaim :: ReadP Discussion
    parseClaim = return Claim

    parseConsent :: ReadP Discussion
    parseConsent = return Consent

    parseRebuttal :: ReadP Discussion
    parseRebuttal = return Rebuttal

    parseSupplement :: ReadP Discussion
    parseSupplement = return Supplement

    parseTitle :: ReadP String
    parseTitle = do
        munch isSpace
        string "title"
        munch1 isSpace
        title <- munch1 isLetter
        return title

    parseDeclartion :: ReadP ( String , Expression )
    parseDeclartion = do
        munch isSpace
        string "define"
        munch isSpace
        variant <- munch1 isLetter
        munch isSpace
        string "="
        munch isSpace
        expression <- parseExpression
        return ( variant , expression )

    parseExpression :: ReadP Expression
    parseExpression = choice [ parseNand , parsePrimary ]

    parseNand :: ReadP Expression
    parseNand = do
        munch isSpace
        expression <- parseExpression
        munch1 isSpace
        string "nand"
        munch1 isSpace
        term <- parsePrimary
        return $ Nand expression term

    parsePrimary :: ReadP Expression
    parsePrimary = do
        munch isSpace
        term <- munch1 isLetter
        return $ Primary term
