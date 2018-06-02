{-# LANGUAGE TemplateHaskell #-}

module Crik.TH.DeriveHttpData
(
  deriveFromHttpData
) where

import Servant.API (FromHttpApiData(parseQueryParam, parseUrlPiece))

import Language.Haskell.TH

deriveFromHttpData :: Name -> Q [Dec]
deriveFromHttpData name = [d|
  instance FromHttpApiData $a where
    parseUrlPiece text = parseUrlPiece text >>= (return . $b)
    parseQueryParam text = parseQueryParam text >>= (return . $b)
  |]
  where
    a = conT name
    b = do
      Just valueName <- lookupValueName (nameBase name)
      conE valueName
