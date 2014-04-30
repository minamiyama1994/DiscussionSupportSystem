{-# OPTIONS -Wall #-}
module DSS.Parser ( Discussion ( Discussion ) , Basiss ( Basiss ) , Label ( Label ) , Basis ( UrlBasis , BookBasis , QuoteBasis ) , Url ( Url ) , Book ( Book ) , Quote ( Quote ) , Isbn ( Isbn ) , Pages ( Pages ) , Claim ( Claim ) , ExpressionString ( StringExpression , QuoteExpression ) , Expression ( Expression ) , parse ) where

    import Control.Applicative
    import Data.List
    import Text.Parser.Combinators
    import qualified Text.Parser.Char as TPC
    import qualified Text.ParserCombinators.ReadP as TPR

    data Discussion = Discussion ( Maybe Expression ) Basiss Claim deriving ( Show , Read , Eq )
    data Basiss = Basiss [ ( Maybe Label , Basis ) ] deriving ( Show , Read , Eq )
    data Label = Label String deriving ( Show , Read , Eq )
    data Basis = UrlBasis Url | BookBasis Book | QuoteBasis Quote deriving ( Show , Read , Eq )
    data Url = Url String deriving ( Show , Read , Eq )
    data Book = Book Isbn ( Maybe Pages ) deriving ( Show , Read , Eq )
    data Quote = Quote [ ExpressionString ] deriving ( Show , Read , Eq )
    data Isbn = Isbn String deriving ( Show , Read , Eq )
    data Pages = Pages [ Int ] deriving ( Show , Read , Eq )
    data Claim = Claim [ ExpressionString ] deriving ( Show , Read , Eq )
    data ExpressionString = StringExpression String | QuoteExpression Expression deriving ( Show , Read , Eq )
    data Expression = Expression [ String ] deriving ( Show , Read , Eq )

    -- |
    -- parse discussion
    -- 
    -- >>> parse "opinion to {hoge.piyo}\ntext\"\"\nclaim \"123{hoge.piyo}\""
    -- [Discussion (Just (Expression ["hoge","piyo"])) (Basiss [(Nothing,QuoteBasis (Quote []))]) (Claim [StringExpression "123",QuoteExpression (Expression ["hoge","piyo"])])]
    -- >>> parse "opinion to {hoge.piyo}\n1:text \"hoge{piyo.foo}bar\"\nurl https://github.com/minamiyama1994\n2 : ISBN 9784798120393 pages ( 1024 , 2048 )\nclaim \"123{hoge.piyo}\""
    -- [Discussion (Just (Expression ["hoge","piyo"])) (Basiss [(Just (Label "1"),QuoteBasis (Quote [StringExpression "hoge",QuoteExpression (Expression ["piyo","foo"]),StringExpression "bar"])),(Nothing,UrlBasis (Url "https://github.com/minamiyama1994")),(Just (Label "2"),BookBasis (Book (Isbn "9784798120393") (Just (Pages [1024,2048]))))]) (Claim [StringExpression "123",QuoteExpression (Expression ["hoge","piyo"])])]
    -- >>> parse "opinion to {hoge.piyo}\n1:text \"hoge{piyo.foo}bar\"\nurl https://github.com/minamiyama1994\n2 : ISBN 9784798120393 pages ( 1024 , 2048 )\n3 : ISBN 9784798120393\nclaim \"123{hoge.piyo}\""
    -- [Discussion (Just (Expression ["hoge","piyo"])) (Basiss [(Just (Label "1"),QuoteBasis (Quote [StringExpression "hoge",QuoteExpression (Expression ["piyo","foo"]),StringExpression "bar"])),(Nothing,UrlBasis (Url "https://github.com/minamiyama1994")),(Just (Label "2"),BookBasis (Book (Isbn "9784798120393") (Just (Pages [1024,2048])))),(Just (Label "3"),BookBasis (Book (Isbn "9784798120393") Nothing))]) (Claim [StringExpression "123",QuoteExpression (Expression ["hoge","piyo"])])]
    parse :: String -> [ Discussion ]
    parse s = nub $ map fst $ TPR.readP_to_S discussion s

    discussion :: TPR.ReadP Discussion
    discussion = Discussion <$> optional opinion <*> basiss <*> claim

    opinion :: TPR.ReadP Expression
    opinion = do
        _ <- many $ TPC.oneOf " \t\r\n"
        _ <- TPC.string "opinion"
        _ <- some $ TPC.oneOf " \t\r\n"
        _ <- TPC.string "to"
        _ <- some $ TPC.oneOf " \t\r\n"
        between ( TPC.string "{" ) ( TPC.string "}" ) expression

    basiss :: TPR.ReadP Basiss
    basiss = Basiss <$> ( some ( ( , ) <$> optional label <*> basis ) )

    label :: TPR.ReadP Label
    label = do
        _ <- many $ TPC.oneOf " \t\r\n"
        l <- some $ TPC.noneOf " \t\r\n"
        _ <- many $ TPC.oneOf " \t\r\n"
        _ <- TPC.string ":"
        return $ Label l

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
        p <- optional pages
        return $ Book i p

    pages :: TPR.ReadP Pages
    pages = do
        _ <- many $ TPC.oneOf " \t\r\n"
        _ <- TPC.string "pages"
        _ <- many $ TPC.oneOf " \t\r\n"
        b <- between ( TPC.string "(" ) ( TPC.string ")" ) page_numbers
        return $ Pages b

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

    string_ :: TPR.ReadP [ ExpressionString ]
    string_ = between ( TPC.string "\"" ) ( TPC.string "\"" ) string_body

    string_body :: TPR.ReadP [ ExpressionString ]
    string_body = do
        c <- many $ choice [ StringExpression <$> ( TPC.noneOf "\"\\{}" >>= return . return ) , StringExpression <$> ( TPC.string "\\{" >> return "{" ) , StringExpression <$> ( TPC.string "\\}" >> return "}" ) , StringExpression <$> ( TPC.string "\\\\" >> return "\\" ) , StringExpression <$> ( TPC.string "\\\"" >> return "\"" ) , QuoteExpression <$> between ( TPC.string "{" ) ( TPC.string "}" ) expression ]
        return $ foldr ( \ e l -> case ( e , l ) of
                ( StringExpression e' , StringExpression l' : ls ) -> StringExpression ( e' ++ l' ) : ls
                _ -> e : l ) [ ] c

    expression :: TPR.ReadP Expression
    expression = do
        f <- some $ TPC.noneOf ".{}"
        post <- some $ TPC.string "." >> ( some $ TPC.noneOf ".{}" )
        return $ Expression $ f : post

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
    unreserved = choice [ alpha , digit , TPC.char '-' , TPC.char '.' , TPC.char '_' , TPC.char '~' ] >>= return . return

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
        s <- some digit
        ss <- many $ do
            _ <- TPC.string "-"
            some digit
        return $ Isbn $ concat $ s : ss
