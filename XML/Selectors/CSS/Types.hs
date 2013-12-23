{-# LANGUAGE DeriveDataTypeable #-}
module XML.Selectors.CSS.Types where

import Data.Typeable
import Data.Data

data Selector = Selector SimpleSelector
                | Combinator SimpleSelector Comb Selector deriving (Show, Typeable, Data)

data Comb = Descendant | Child | FollowingSibling | AnySibling deriving (Show, Typeable, Data)

data SimpleSelector = SimpleSelector (Maybe String) [Specifier] (Maybe Pseudo)
                    | Universal deriving (Show, Typeable, Data)

data Specifier = ID String | Class String | Attrib String Pred deriving (Show, Typeable, Data)

data Pred = None
            | Pred PredOp String deriving (Show, Typeable, Data)

data PredOp = Equals | Includes | DashMatch | BeginsWith | EndsWith deriving (Show, Typeable, Data)

data Pseudo = FirstChild | LastChild deriving (Show, Typeable, Data)

data Token = TokenSpace |
             TokenName String |
             TokenString String |
             TokenPlus |
             TokenMinus |
             TokenSlash |
             TokenAster |
             TokenChild |
             TokenNthChild |
             TokenFirstChild |
             TokenLastChild |
             TokenNthLastChild |
             TokenAnySibling |
             TokenOB |
             TokenCB |
             TokenOP |
             TokenCP |
             TokenDigits String |
             TokenHash |
             TokenDot |
             TokenEquals |
             TokenIncludes |
             TokenDashMatch |
             TokenBeginsWith |
             TokenEndsWith |
             TokenQuote |
             TokenEOF |
             TokenPseudo deriving (Eq, Show)
