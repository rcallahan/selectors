{ module XML.Selectors.CSS.Parse (parsePath) where

import XML.Selectors.CSS.Tokens (lexer)
import XML.Selectors.CSS.Types
}

%name cssPath
%tokentype { Token }
%error { parseError }
%monad { Either String }
%token
    sp                      { TokenSpace }
    name                    { TokenName $$ }
    string                  { TokenString $$ }
    '+'                     { TokenPlus }
    '-'                     { TokenMinus }
    '/'                     { TokenSlash }
    '>'                     { TokenChild }
    '~'                     { TokenAnySibling }
    '#'                     { TokenHash }
    firstchild              { TokenFirstChild }
    lastchild               { TokenLastChild }
    '.'                     { TokenDot }
    '='                     { TokenEquals }
    "~="                    { TokenIncludes }
    "^="                    { TokenBeginsWith }
    "$="                    { TokenEndsWith }
    "|="                    { TokenDashMatch }
    ':'                     { TokenPseudo }
    '('                     { TokenOP }
    ')'                     { TokenCP }
    '['                     { TokenOB }
    ']'                     { TokenCB }
    nat                     { TokenDigits $$ }
    '*'                     { TokenAster }

%%

Selector    : SimpleSelector                        { Selector $1 }
            | SimpleSelector Combinator Selector    { Combinator $1 $2 $3 }

Combinator  : sp                                    { Descendant }
            | sp Combinator                         { $2 }
            | '+'                                   { FollowingSibling }
            | '+' sp                                { FollowingSibling }
            | '~'                                   { AnySibling }
            | '~' sp                                { AnySibling }
            | '>'                                   { Child }
            | '>' sp                                { Child }

SimpleSelector  : name                              { SimpleSelector (Just $1) [] Nothing }
                | name specs                        { SimpleSelector (Just $1) $2 Nothing }
                | name specs Pseudo                 { SimpleSelector (Just $1) $2 (Just $3) }
                | specs                             { SimpleSelector Nothing $1 Nothing }
                | specs Pseudo                      { SimpleSelector Nothing $1 (Just $2) }
                | '*'                               { SimpleSelector Nothing [] Nothing }

specs   : Specifier                                 { [$1] }
        | specs Specifier                           { $2 : $1 }

Specifier   : '#' name                              { ID $2 }
            | '.' name                              { Class $2 }
            | '[' attr ']'                          { $2 }
            | '[' attr sp ']'                       { $2 }

attr    : name Pred                                 { Attrib $1 $2 }
        | sp attr                                   { $2 }

Pred    : sp Pred                                   { $2 }
        | PredOp sp string                          { Pred $1 $3 }
        | PredOp string                             { Pred $1 $2 }

PredOp  : '='                                       { Equals }
        | "~="                                      { Includes }
        | "|="                                      { DashMatch }
        | "^="                                      { BeginsWith }
        | "$="                                      { EndsWith }

Pseudo  : ':' firstchild                            { FirstChild }
        | ':' lastchild                             { LastChild }

{
parseError toks = Left $ "parse error: " ++ show toks

parsePath :: String -> Either String Selector
parsePath str = lexer str >>= cssPath
}
