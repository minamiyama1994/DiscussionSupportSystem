module DSS.Html ( generateHtml , Html ( Html ) ) where

    import DSS.Parser
    import Text.Html ( stringToHtmlString )

    data Html = Html String

    generateHtml :: Discussion -> String -> Html
    generateHtml ( Discussion opinion basiss claim ) baseUrl = Html $ concat [ gen baseUrl | gen <- [ generateOpinion opinion , generateBasiss basiss , generateClaim claim ] ]

    generateOpinion :: Maybe Expression -> String -> String
    generateOpinion ( Just expr ) baseUrl = "<h1>to " ++ generateExpression expr baseUrl ++ "</h1>"
    generateOpinion Nothing _ = "<h1>Begin discussion</h1>"

    generateBasiss :: Basiss -> String -> String
    generateBasiss ( Basiss pairs ) baseUrl = "<p>My basis's is <ul>" ++ concat ( map ( \ p -> generateBasis p baseUrl ) pairs ) ++ "</ul></p>"

    generateBasis :: ( Maybe Label , Basis ) -> String -> String
    generateBasis ( l , b ) baseUrl = "<li" ++ maybe "" ( \ ( Label s ) -> " id=\"" ++ stringToHtmlString s ++ "\"" ) l ++ ">" ++ maybe "" ( \ ( Label s ) -> "<a href=\"#" ++ stringToHtmlString s ++ "\">" ++ stringToHtmlString s ++ "</a><br />" ) l ++ generateBasis' b baseUrl ++ "</li>"

    generateBasis' :: Basis -> String -> String
    generateBasis' ( UrlBasis ( Url s ) ) _ = "<a href=\"" ++ stringToHtmlString s ++ "\">" ++ stringToHtmlString s ++ "</a>"
    generateBasis' ( BookBasis ( Book ( Isbn isbn ) pages ) ) _ = "book of ISBN " ++ stringToHtmlString isbn ++ maybe "" ( \ ( Pages ps ) -> "<br />pages of " ++ stringToHtmlString ( concat ( show ( head ps ) : [ ',' : show s | s <- tail ps ] ) ) ) pages
    generateBasis' ( QuoteBasis ( Quote exprs ) ) baseUrl = concat $ map ( \ e -> generateExpressionString e baseUrl ) exprs

    generateExpressionString :: ExpressionString -> String -> String
    generateExpressionString ( StringExpression s ) _ = stringToHtmlString s
    generateExpressionString ( QuoteExpression expr ) baseUrl = generateExpression expr baseUrl

    generateExpression :: Expression -> String -> String
    generateExpression ( Expression exprs ) baseUrl = "<a href=\"" ++ stringToHtmlString ( baseUrl ++ concat [ '/' : s | s <- init exprs ] ++ ( '#' : last exprs ) ) ++ "\">" ++ stringToHtmlString ( concat ( head exprs : [ '.' : s | s <- tail exprs ] ) ) ++ "</a>"

    generateClaim :: Claim -> String -> String
    generateClaim ( Claim exprs ) baseUrl = "My claim is<br /><strong>" ++ ( concat $ map ( \ e -> generateExpressionString e baseUrl ) exprs ) ++ "</strong>"
