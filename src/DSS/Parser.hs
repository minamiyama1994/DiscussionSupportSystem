{-# OPTIONS -Wall #-}
module DSS.Parser ( Discussion , Basiss , Basis , Url , Book , Quote , Isbn , Pages , Claim , parse ) where

    import Control.Applicative
    import Data.List
    import Text.Parser.Combinators
    import qualified Text.Parser.Char as TPC
    import qualified Text.ParserCombinators.ReadP as TPR

    data Discussion = Discussion Basiss Claim deriving ( Show , Eq )
    data Basiss = Basiss [ Basis ] deriving ( Show , Eq )
    data Basis = UrlBasis Url | BookBasis Book | QuoteBasis Quote deriving ( Show , Eq )
    data Url = Url String deriving ( Show , Eq )
    data Book = Book Isbn Pages deriving ( Show , Eq )
    data Quote = Quote String deriving ( Show , Eq )
    data Isbn = Isbn String deriving ( Show , Eq )
    data Pages = Pages [ Int ] deriving ( Show , Eq )
    data Claim = Claim String deriving ( Show , Eq )

    parse :: String -> [ Discussion ]
    parse s = nub $ map fst $ TPR.readP_to_S discussion s

    discussion :: TPR.ReadP Discussion
    discussion = Discussion <$> basiss <*> claim

    basiss :: TPR.ReadP Basiss
    basiss = Basiss <$> ( some $ basis )

    basis :: TPR.ReadP Basis
    basis = choice [ UrlBasis <$> url , BookBasis <$> book , QuoteBasis <$> quote_discussion ]

    url :: TPR.ReadP Url
    url = do
        _ <- many $ TPC.oneOf " \t\r\n"
        _ <- TPC.string "url"
        _ <- many $ TPC.oneOf " \t\r\n"
        u <- uri
        return $ Url u

    book :: TPR.ReadP Book
    book = do
        _ <- many $ TPC.oneOf " \t\r\n"
        _ <- TPC.string "ISBN"
        i <- isbn
        p <- pages
        return $ Book i p

    pages :: TPR.ReadP Pages
    pages = do
        _ <- many $ TPC.oneOf " \t\r\n"
        _ <- TPC.string "pages"
        b <- between ( TPC.string "(" ) ( TPC.string ")" ) ( optional page_numbers )
        return $ Pages $ maybe [ ] id b

    page_numbers :: TPR.ReadP [ Int ]
    page_numbers = do
        _ <- many $ TPC.oneOf " \t\r\n"
        p <- page_number
        _ <- many $ TPC.oneOf " \t\r\n"
        m <- many $ do
            _ <- many $ TPC.oneOf " \t\r\n"
            _ <- TPC.string ","
            _ <- many $ TPC.oneOf " \t\r\n"
            p' <- page_number
            _ <- many $ TPC.oneOf " \t\r\n"
            return p'
        return $ p : m

    page_number :: TPR.ReadP Int
    page_number = do
        d <- digit_
        return $ read d

    quote_discussion :: TPR.ReadP Quote
    quote_discussion = do
        _ <- many $ TPC.oneOf " \t\r\n"
        _ <- TPC.string "text"
        _ <- many $ TPC.oneOf " \t\r\n"
        s <- string_
        return $ Quote s

    claim :: TPR.ReadP Claim
    claim = do
        _ <- many $ TPC.oneOf " \t\r\n"
        _ <- TPC.string "claim"
        _ <- many $ TPC.oneOf " \t\r\n"
        s <- string_
        return $ Claim s

    digit_ :: TPR.ReadP String
    digit_ = some $ TPC.oneOf [ '0' .. '9' ]

    string_ :: TPR.ReadP String
    string_ = do
        b <- between ( TPC.string "\"" ) ( TPC.string "\"" ) $ string_body
        return $ b

    string_body :: TPR.ReadP String
    string_body = do
        c <- many $ choice [ TPC.noneOf "\"\\" , do
            _ : s : [ ] <- choice [ TPC.string "\\\\" , TPC.string "\\\"" ]
            return s ]
        return $ c

    uri :: TPR.ReadP String
    uri = do
        sc <- scheme
        colon <- TPC.string ":"
        hp <- hier_part
        q <- optional $ do
            q' <- TPC.string "?"
            q'' <- query
            return $ q' ++ q''
        f <- optional $ do
            h <- TPC.string "#"
            f' <- fragment
            return $ h ++ f'
        return $ sc ++ colon ++ hp ++ ( maybe "" id q ) ++ ( maybe "" id f )

    hier_part :: TPR.ReadP String
    hier_part = choice [ do {
        ss <- TPC.string "//" ;
        a <- authority ;
        p <- path_abempty ;
        return $ ss ++ a ++ p } , path_absolute , path_rootless , path_empty ]

    scheme :: TPR.ReadP String
    scheme = do
        a <- alpha
        m <- many $ choice [ alpha , digit , TPC.char '+' , TPC.char '-' , TPC.char '.' ]
        return $ a : m

    authority :: TPR.ReadP String
    authority = do
        s <- optional $ do
            u <- userinfo
            a <- TPC.string "@"
            return $ u ++ a
        h <- host
        p <- optional $ do
            c <- TPC.string ":"
            p <- port
            return $ c ++ p
        return $ maybe "" id s ++ h ++ maybe "" id p

    userinfo :: TPR.ReadP String
    userinfo = ( many $ choice [ unreserved , pct_encoded , sub_delims , TPC.string ":" ] ) >>= return . concat

    host :: TPR.ReadP String
    host = choice [ ip_literal , ipv4address , reg_name ]

    port :: TPR.ReadP String
    port = many digit

    ip_literal :: TPR.ReadP String
    ip_literal = do
        l <- TPC.string "["
        a <- choice [ ipv6address , ipvfuture ]
        r <- TPC.string "]"
        return $ l ++ a ++ r

    ipvfuture :: TPR.ReadP String
    ipvfuture = do
        v <- TPC.string "v"
        h <- some hexdig
        d <- TPC.string "."
        s <- some $ choice [ unreserved , sub_delims , TPC.string ":" ]
        return $ v ++ h ++ d ++ concat s

    ipv6address :: TPR.ReadP String
    ipv6address = choice [ do
        r <- rep 6 6 $ do
            h <- h16
            c <- TPC.string ":"
            return $ h ++ c
        l <- ls32
        return $ concat r ++ l , do

        cc <- TPC.string "::"
        r <- rep 5 5 $ do
            h <- h16
            c <- TPC.string ":"
            return $ h ++ c
        l <- ls32
        return $ cc ++ concat r ++ l , do
        o <- optional h16

        s <- TPC.string "::"
        r <- rep 4 4 $ do
            h <- h16
            s' <- TPC.string ":"
            return $ h ++ s'
        l <- ls32
        return $ maybe "" id o ++ s ++ concat r ++ l , do
        o <- optional $ do
            r <- rep 0 1 $ do
                h <- h16
                s <- TPC.string ":"
                return $ h ++ s
            h <- h16
            return $ concat r ++ h

        s <- TPC.string "::"
        r <- rep 3 3 $ do
            h <- h16
            s' <- TPC.string ":"
            return $ h ++ s'
        l <- ls32
        return $ maybe "" id o ++ s ++ concat r ++ l , do
        o <- optional $ do
            r <- rep 0 2 $ do
                h <- h16
                s <- TPC.string ":"
                return $ h ++ s
            h <- h16
            return $ concat r ++ h

        s <- TPC.string "::"
        r <- rep 2 2 $ do
            h <- h16
            s' <- TPC.string ":"
            return $ h ++ s'
        l <- ls32
        return $ maybe "" id o ++ s ++ concat r ++ l , do
        o <- optional $ do
            r <- rep 0 3 $ do
                h <- h16
                s <- TPC.string ":"
                return $ h ++ s
            h <- h16
            return $ concat r ++ h

        s <- TPC.string "::"
        h <- h16
        s' <- TPC.string ":"
        l <- ls32
        return $ maybe "" id o ++ s ++ h ++ s' ++ l , do
        o <- optional $ do
            r <- rep 0 4 $ do
                h <- h16
                s <- TPC.string ":"
                return $ h ++ s
            h <- h16
            return $ concat r ++ h

        s <- TPC.string "::"
        l <- ls32
        return $ maybe "" id o ++ s ++ l , do
        o <- optional $ do
            r <- rep 0 5 $ do
                h <- h16
                s <- TPC.string ":"
                return $ h ++ s
            h <- h16
            return $ concat r ++ h

        s <- TPC.string "::"
        h <- h16
        return $ maybe "" id o ++ s ++ h , do
        o <- optional $ do
            r <- rep 0 6 $ do
                h <- h16
                s <- TPC.string ":"
                return $ h ++ s
            h <- h16
            return $ concat r ++ h

        s <- TPC.string "::"
        return $ maybe "" id o ++ s ]

    h16 :: TPR.ReadP  String
    h16 = rep 1 4 hexdig

    ls32 :: TPR.ReadP String
    ls32 = choice [ do
        h <- h16
        s <- TPC.string ":"
        h' <- h16
        return $ h ++ s ++ h' , ipv4address ]

    ipv4address :: TPR.ReadP String
    ipv4address = do
        d <- dec_octet
        s <- TPC.string "."
        d' <- dec_octet
        s' <- TPC.string "."
        d'' <- dec_octet
        s'' <- TPC.string "."
        d''' <- dec_octet
        return $ d ++ s ++ d' ++ s' ++ d'' ++ s'' ++ d'''

    dec_octet :: TPR.ReadP String
    dec_octet = choice [ digit >>= \ x -> return [ x ] , do
        o <- TPC.oneOf [ '\x31' .. '\x39' ]
        d <- digit
        return $ [ o , d ] , do
        s <- TPC.string "1"
        r <- rep 2 2 digit
        return $ s ++ r , do
        s <- TPC.string "2"
        o <- TPC.oneOf [ '\x30' .. '\x34' ]
        d <- digit
        return $ s ++ [ o ] ++ [ d ] , do
        s <- TPC.string "25"
        o <- TPC.oneOf [ '\x30' .. '\x35' ]
        return $ s ++ [ o ] ]

    reg_name :: TPR.ReadP String
    reg_name = ( many $ choice [ unreserved , pct_encoded , sub_delims ] ) >>= return . concat

    path_abempty :: TPR.ReadP String
    path_abempty = do
        m <- many $ do
            s <- TPC.string "/"
            s'<- segment
            return $ s ++ s'
        return $ concat m

    path_absolute :: TPR.ReadP String
    path_absolute = do
        s <- TPC.string "/"
        o <- optional $ do
            s' <- segment_nz
            m <- many $ do
                s'' <- TPC.string "/"
                s''' <- segment
                return $ s'' ++ s'''
            return $ s' ++ concat m
        return $ s ++ maybe "" id o

    path_rootless :: TPR.ReadP String
    path_rootless = do
        s <- segment_nz
        m <- many $ do
            s' <- TPC.string "/"
            s'' <- segment
            return $ s' ++ s''
        return $ s ++ concat m

    path_empty :: TPR.ReadP String
    path_empty = rep 0 0 pchar >>= return . concat

    segment :: TPR.ReadP String
    segment = many pchar >>= return . concat

    segment_nz :: TPR.ReadP String
    segment_nz = some pchar >>= return . concat

    pchar :: TPR.ReadP String
    pchar = choice [ unreserved , pct_encoded , sub_delims , TPC.string ":" , TPC.string "@" ]

    query :: TPR.ReadP String
    query = ( many $ choice [ pchar , TPC.string "/" , TPC.string "?" ] ) >>= return . concat

    fragment :: TPR.ReadP String
    fragment = ( many $ choice [ pchar , TPC.string "/" , TPC.string "?" ] ) >>= return . concat

    pct_encoded :: TPR.ReadP String
    pct_encoded = do
        s <- TPC.string "%"
        h <- hexdig
        h' <- hexdig
        return $ s ++ [ h ] ++ [ h' ]

    unreserved :: TPR.ReadP String
    unreserved = choice [ alpha , digit , TPC.char '-' , TPC.char '.' , TPC.char '_' , TPC.char '~' ] >>= \ x -> return [ x ]

    sub_delims :: TPR.ReadP String
    sub_delims = choice [ TPC.string x | x <- [ "!" , "$" , "&" , "'" , "(" , ")" , "*" , "+" , "," , ";" , "=" ] ]

    alpha :: TPR.ReadP Char
    alpha = TPC.oneOf $ [ '\x41' .. '\x5A' ] ++ [ '\x61' .. '\x7A' ]

    digit :: TPR.ReadP Char
    digit = TPC.oneOf $ [ '\x30' .. '\x39' ]

    hexdig :: TPR.ReadP Char
    hexdig = TPC.oneOf $ [ '0' .. '9' ] ++ [ 'A' .. 'F' ] ++ [ 'a' .. 'f' ]

    rep :: Int -> Int -> TPR.ReadP a -> TPR.ReadP [ a ]
    rep n m r = choice [ count x r | x <- [ n .. m ] ]

    isbn :: TPR.ReadP Isbn
    isbn = do
        _ <- many $ TPC.oneOf " \t\r\n"
        s0 <- some digit
        s1 <- TPC.string "-"
        s2 <- some digit
        s3 <- TPC.string "-"
        s4 <- some digit
        s5 <- TPC.string "-"
        s6 <- some digit
        s7 <- TPC.string "-"
        s8 <- some digit
        return $Isbn $ s0 ++ s1 ++ s2 ++ s3 ++ s4 ++ s5 ++ s6 ++ s7 ++ s8