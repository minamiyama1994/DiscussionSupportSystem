module DSS.Parser where

    import Text.Regex
    import Text.Regex.Posix
    import Data.List
    import Text.ParserCombinators.ReadP

    data Discussion = Claim | Consent | Correction | Rebuttal | Supplement deriving ( Show )

    parse :: ReadS Discussion
    parse s = ( readP_to_S $ choice $ [ parseClaim , parseConsent , parseRebuttal , parseSupplement ] ) s

    parseClaim :: ReadP Discussion
    parseClaim = return Claim

    parseConsent :: ReadP Discussion
    parseConsent = return Consent

    parseRebuttal :: ReadP Discussion
    parseRebuttal = return Rebuttal

    parseSupplement :: ReadP Discussion
    parseSupplement = return Supplement
