module DSS.Parser where
    data Category = ClaimCategory Claim | ConsentCategory Consent | CorrectionCategory Correction | RebuttalCategory Rebuttal | SupplementCategory Supplement | Tree deriving ( Show )
    data Claim = Claim deriving ( Show )
    data Consent = Consent deriving ( Show )
    data Correction = Correction deriving ( Show )
    data Rebuttal = Rebuttal deriving ( Show )
    data Supplement = Supplement deriving ( Show )
    parse :: String -> Category
    parse _ = Tree