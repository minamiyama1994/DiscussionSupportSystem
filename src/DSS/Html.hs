{-# LANGUAGE FlexibleInstances #-}
module DSS.Html where

    import Control.Monad
    import DSS.Parser
    import Text.Html ( stringToHtmlString )
    import Text.Blaze
    import Text.Blaze.Html5
    import Text.Blaze.Html5.Attributes

    instance ToMarkup ( String , Discussion ) where
        toMarkup ( baseUrl , Discussion opinion basiss claim ) = do
            toMarkup ( baseUrl , opinion )
            toMarkup ( baseUrl , basiss )
            toMarkup ( baseUrl , claim )

    instance ToMarkup ( String , Maybe Expression ) where
        toMarkup ( baseUrl , Just expr ) = h1 $ do
            toMarkup "to "
            toMarkup ( baseUrl , expr )
        toMarkup ( _ , Nothing ) = h1 $ do
            toMarkup "Begin discussion"

    instance ToMarkup ( String , Basiss ) where
        toMarkup ( baseUrl , Basiss pairs ) = p $ do
            toMarkup "My basis's is "
            ul $ do
                mapM_ ( toMarkup . ( , ) baseUrl ) pairs

    instance ToMarkup ( String ,( Maybe Label , Basis ) ) where
        toMarkup ( baseUrl , ( Just l , b ) ) = li ! ( \ ( Label s ) -> Text.Blaze.Html5.Attributes.id $ toValue . stringToHtmlString $ s ) l $ do
            a ! href ( ( \ ( Label s ) -> toValue . stringToHtmlString $ s ) l ) $ ( ( \ ( Label s ) -> toMarkup . stringToHtmlString $ s ) l ) 
            br
            toMarkup ( baseUrl , b )
        toMarkup ( baseUrl , ( Nothing , b ) ) = li $ do
            toMarkup ( baseUrl , b )

    instance ToMarkup ( String , Basis ) where
        toMarkup ( _ , UrlBasis ( Url s ) ) = a ! href ( toValue $ stringToHtmlString s ) $ toMarkup $ stringToHtmlString s
        toMarkup ( _ , BookBasis ( Book ( Isbn isbn ) pages ) ) = do
            toMarkup $ "book of ISBN " ++ stringToHtmlString isbn
            br
            maybe ( toMarkup "" ) ( \ ( Pages ps ) -> do
                br
                toMarkup $ "pages of " ++ stringToHtmlString ( concat ( show ( Prelude.head ps ) : [ ',' : show s | s <- tail ps ] ) ) ) pages
        toMarkup ( baseUrl , QuoteBasis ( Quote exprs ) ) = toMarkup $ mapM_ ( toMarkup . ( , ) baseUrl ) exprs

    instance ToMarkup ( String , ExpressionString ) where
        toMarkup ( _ , StringExpression s ) = toMarkup s
        toMarkup ( baseUrl , ( QuoteExpression expr ) ) = toMarkup ( baseUrl , expr )

    instance ToMarkup ( String , Expression ) where
        toMarkup ( baseUrl , Expression [ name , discussion ] ) = a ! href ( toValue $ stringToHtmlString ( baseUrl ++ '/' : name ++ '/' : discussion ) ) $ toMarkup $ stringToHtmlString ( name ++ '.' : discussion )
        toMarkup ( baseUrl , Expression [ name , discussion , label ] ) = a ! href ( toValue $ stringToHtmlString ( baseUrl ++ '/' : name ++ '/' : discussion ++ '#' : label ) ) $ toMarkup $ stringToHtmlString ( name ++ '.' : discussion ++ '.' : label )

    instance ToMarkup ( String , Claim ) where
        toMarkup ( baseUrl , Claim exprs ) = do
            toMarkup "My claim is"
            br
            strong $ do
                mapM_ ( toMarkup . ( , ) baseUrl ) exprs
