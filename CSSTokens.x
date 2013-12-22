{
module CSSTokens where
}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = [0-9]
$nonascii = [\x80-\x10ffff]
$namestart = [$alpha _ $nonascii]
$name = [$namestart \- $digit]

tokens :-

    $white                  { \s -> TokenSpace }
    $namestart $name*       { \s -> TokenName s }
    \+                      { \s -> TokenSibling }
    \>                      { \s -> TokenChild }
    \~                      { \s -> TokenAnySibling }
    \# $name*               { \(_:s) -> TokenHash s }
    \.                      { \s -> TokenDot }
    \=                      { \s -> TokenEquals }
    \~=                     { \s -> TokenIncludes }
    \|=                     { \s -> TokenDashMatch }
    \^=                     { \s -> TokenBeginsWith }
    \$=                     { \s -> TokenEndsWith }
    \:                      { \s -> TokenPseudo }
    \[                      { \s -> TokenOB }
    \]                      { \s -> TokenCB }
    \(                      { \s -> TokenOP }
    \)                      { \s -> TokenCP }
    \"                      { \s -> TokenQuote }
    \*                      { \s -> TokenUniversal }

{

data Token = TokenSpace |
             TokenName String |
             TokenChild |
             TokenSibling |
             TokenAnySibling |
             TokenOB |
             TokenCB |
             TokenOP |
             TokenCP |
             TokenHash String |
             TokenDot |
             TokenEquals |
             TokenIncludes |
             TokenDashMatch |
             TokenBeginsWith |
             TokenEndsWith |
             TokenQuote |
             TokenUniversal |
             TokenPseudo deriving (Eq, Show)

lexer :: String -> Either String [Token]
lexer str = go ('\n',[],str) where
    go inp@(_,_bs,s) = case alexScan inp 0 of
            AlexEOF -> return []
            AlexError er -> Left $ "lexical error: " ++ show er
            AlexSkip  inp' len     -> go inp'
            AlexToken inp' len act -> do
                let this = act (take len s)
                rest <- go inp'
                return $ this : rest

}

