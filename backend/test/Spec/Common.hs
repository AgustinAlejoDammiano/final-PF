module Spec.Common where

import ClassyPrelude
import Text.StringRandom
import Feature.Jurisdiction.Types

randomCreateJurisdictionParam :: Integer -> IO CreateJurisdiction
randomCreateJurisdictionParam id = do
  name <- stringRandomIO "[a-zA-z0-9 ]{10}"
  return $ CreateJurisdiction id name

getJurisdictionFromParam :: CreateJurisdiction -> Jurisdiction
getJurisdictionFromParam j = Jurisdiction (createJurisdictionId j) (createJurisdictionName j)
