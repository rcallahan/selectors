{ module XML.Selectors.CSS.Parse where

import XML.Selectors.CSS.Tokens (Token(..), lexer)
}

%name cssPath
%tokentype { Token }
%error { parseError }
%monad { Either String }
%token
    sp                      { TokenSpace }
    name                    { TokenName $$ }
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
    '"'                     { TokenQuote }
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
            | '+'                                   { FollSibling }
            | '+' sp                                { FollSibling }
            | '~'                                   { AnyFollSibling }
            | '~' sp                                { AnyFollSibling }
            | '>'                                   { Child }
            | '>' sp                                { Child }

SimpleSelector  : name                              { SimpleSelector (Just $1) [] Nothing }
                | name specs                        { SimpleSelector (Just $1) $2 Nothing }
                | name specs Pseudo                 { SimpleSelector (Just $1) $2 (Just $3) }
                | specs                             { SimpleSelector Nothing $1 Nothing }
                | specs Pseudo                      { SimpleSelector Nothing $1 (Just $2) }
                | '*'                               { Universal }

specs   : Specifier                                 { [$1] }
        | specs Specifier                           { $2 : $1 }

Specifier   : '#' name                              { ID $2 }
            | '.' name                              { Class $2 }
            | '[' attr ']'                          { $2 }

attr    : name Pred                                 { Attrib $1 $2 }
        | sp attr                                   { $2 }

Pred    : '=' spacedname                            { Equals $2 }
        | "~=" spacedname                           { Includes $2 }
        | "|=" spacedname                           { DashMatch $2 }
        | "^=" spacedname                           { BeginsWith $2 }
        | "$=" spacedname                           { EndsWith $2 }
        | sp Pred                                   { $2 }
        |                                           { None }

Pseudo  : ':' firstchild                            { FirstChild }
        | ':' lastchild                             { LastChild }

spacedname  : sp quotedname sp                      { $2 }
            | quotedname                            { $1 }
            | quotedname sp                         { $1 }

quotedname  : '"' name '"'                          { $2 }

{
parseError toks = Left $ "parse error: " ++ show toks

data Selector = Selector SimpleSelector
                | Combinator SimpleSelector Comb Selector deriving Show

data Comb = Descendant | Child | FollSibling | AnyFollSibling deriving Show

data SimpleSelector = SimpleSelector (Maybe String) [Specifier] (Maybe Pseudo)
                    | Universal deriving Show

data Specifier = ID String | Class String | Attrib String Pred deriving Show

data Pred = None
            | Equals String
            | Includes String
            | DashMatch String
            | BeginsWith String
            | EndsWith String deriving Show

data Pseudo = FirstChild | LastChild deriving Show

parsePath str = lexer str >>= cssPath
}
