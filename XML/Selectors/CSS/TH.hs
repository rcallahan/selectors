{-# LANGUAGE TemplateHaskell #-}

module XML.Selectors.CSS.TH (css) where

import XML.Selectors.CSS.Parse
import XML.Selectors.CSS
import Language.Haskell.TH.Quote
import Language.Haskell.TH

css :: QuasiQuoter
css = QuasiQuoter { quoteExp = cssExp, quotePat = undefined, quoteDec = undefined, quoteType = undefined }

cssExp s = appE [|toAxis |] $ either fail (dataToExpQ $ const Nothing) $ parsePath s

