-- | Definitions of 'Token's in the CSS selector grammar as well as the data types
-- representing the parse result.
{-# LANGUAGE DeriveDataTypeable #-}
module XML.Selectors.CSS.Types where

import Data.Typeable
import Data.Data

-- | Top level entity representing the parse tree of a CSS path expression.
-- Composed of one or more 'SimpleSelector's combined with a 'Comb' relation.
data Selector   = Selector SimpleSelector
                | Combinator SimpleSelector Comb Selector
    deriving (Show, Typeable, Data)

-- | Type representing the relationship between two 'Selector's.
data Comb 
            -- | Any descendant; represented by plain whitespace in CSS
            = Descendant
            -- | Any direct child; represented by @ > @ in CSS
            | Child
            -- | Directly following sibling node; represented by @ + @ in CSS
            | FollowingSibling
            -- | Any following sibling node; represented by @ ~ @ in CSS
            | AnySibling
    deriving (Show, Typeable, Data)

-- | Type representing a single set of filters for selecting nodes.
-- Contain an optional single element name, a sequence of id, class, and attribute
-- 'Specifier's, and an optional pseudo-element selector.
data SimpleSelector = SimpleSelector (Maybe String) [Specifier] (Maybe Pseudo)
                    -- | Wildcard @ * @ in CSS
                    | Universal
    deriving (Show, Typeable, Data)


-- | Type representing id, class, and attribute filters.
data Specifier  = ID String
                | Class String
                | Attrib String Pred
    deriving (Show, Typeable, Data)

-- | Type representing boolean operations on attribute values.
data Pred   
            -- | Simple existence test
            = None
            -- | String comparison test
            | Pred PredOp String
    deriving (Show, Typeable, Data)

data PredOp 
            -- | String equality
            = Equals
            -- | Contains word; @ ~= @
            | Includes
            -- | String equality, optional dash following; @ |= @
            | DashMatch
            -- | String begins with; @ ^= @
            | BeginsWith
            -- | String ends with; @ $= @
            | EndsWith
    deriving (Show, Typeable, Data)

-- | Only supporting two pseudoelement selectors.
data Pseudo = FirstChild
            | LastChild
    deriving (Show, Typeable, Data)

data Token  = TokenSpace
            | TokenName String
            | TokenString String
            | TokenPlus
            | TokenMinus
            | TokenSlash
            | TokenAster
            | TokenChild
            | TokenNthChild
            | TokenFirstChild
            | TokenLastChild
            | TokenNthLastChild
            | TokenAnySibling
            | TokenOB
            | TokenCB
            | TokenOP
            | TokenCP
            | TokenDigits String
            | TokenHash
            | TokenDot
            | TokenEquals
            | TokenIncludes
            | TokenDashMatch
            | TokenBeginsWith
            | TokenEndsWith
            | TokenQuote
            | TokenEOF
            | TokenPseudo
    deriving (Eq, Show)
