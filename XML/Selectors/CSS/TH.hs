{-# LANGUAGE TemplateHaskell #-}

module XML.Selectors.CSS.TH (css) where

import XML.Selectors.CSS.Parse
import XML.Selectors.CSS
import Language.Haskell.TH.Quote
import Language.Haskell.TH

-- | @ [css| #interface div.doc:first-child |] @ would select this paragraph.
css :: QuasiQuoter
css = QuasiQuoter { quoteExp = cssExp, quotePat = undefined, quoteDec = undefined, quoteType = undefined }

cssExp s = appE [|toAxis |] $ either fail (dataToExpQ $ const Nothing) $ parsePath s

