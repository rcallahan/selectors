{
module XML.Selectors.CSS.Tokens where

import XML.Selectors.CSS.Types
}

%wrapper "monad"

$alpha = [a-zA-Z]
$digit = [0-9]
$nonascii = [\x80-\x10ffff]
$namestart = [$alpha _ $nonascii]
$namechar = [$namestart \- $digit]
@nat = [0-9]+
@name = $namestart $namechar*
@hashname = $namechar+
@string1 = [^\"]*
@string2 = [^\']*


tokens :-

    $white                  { mkT TokenSpace }
    ^ @name                 { \(_,_,_,s) l -> return $ TokenName (take l s) }
    [^\#\:]^ @name          { \(_,_,_,s) l -> return $ TokenName (take l s) }
    \+                      { mkT TokenPlus }
    \-                      { mkT TokenMinus }
    \*                      { mkT TokenAster }
    \/                      { mkT TokenSlash }
    \>                      { mkT TokenChild }
    \~                      { mkT TokenAnySibling }
    \#                      { mkT TokenHash }
    \:^first\-child         { mkT TokenFirstChild }
    \:^last\-child          { mkT TokenLastChild }
    \:^nth\-child           { mkT TokenNthChild }
    \:^nth\-last\-child     { mkT TokenNthLastChild }
    \#^ @hashname           { \(_,_,_,s) l -> return $ TokenName (take l s) }
    \.                      { mkT TokenDot }
    \=                      { mkT TokenEquals }
    \~=                     { mkT TokenIncludes }
    \|=                     { mkT TokenDashMatch }
    \^=                     { mkT TokenBeginsWith }
    \$=                     { mkT TokenEndsWith }
    \:                      { mkT TokenPseudo }
    \[                      { mkT TokenOB }
    \]                      { mkT TokenCB }
    \" @string1 \"          { \(_,_,_,(_:s)) l -> return $ TokenString $ take (l-2) s }
    \' @string2 \'          { \(_,_,_,(_:s)) l -> return $ TokenString $ take (l-2) s }
    <0> \(                  { \_ _ -> alexSetStartCode expr >> return TokenOP }
    <expr> \)               { \_ _ -> alexSetStartCode 0 >> return TokenCP }
    <expr> @nat             { \(_,_,_,s) l -> return $ TokenDigits (take l s) }

{

mkT p _ _ = return p

alexEOF = return TokenEOF

lexer str = runAlex str go where
    go = do 
        tok <- alexMonadScan; 
        if tok == TokenEOF then return [] else do
            rest <- go
            return $ tok : rest
}

