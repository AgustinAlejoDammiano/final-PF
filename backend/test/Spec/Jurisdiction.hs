module Spec.Jurisdiction (spec) where

import ClassyPrelude
import Test.Hspec
import Feature.Jurisdiction.Types
import Spec.Common
import qualified Utils.Client as Client

spec :: Spec
spec = do
    jurisdictionSpec


jurisdictionSpec :: Spec
jurisdictionSpec =
    describe "jurisdictions" $ do

        describe "create jurisdictions" $ do

            it "should create jurisdiction successfully" $ do
                param <- randomCreateJurisdictionParam 1 
                Right jurisdiction' <- Client.runClient $ Client.createJurisdiction param
                (getJurisdictionFromParam param) `shouldBe` jurisdiction'

--             Left (Client.ErrApp a) -> handlerJuristictionError a
--             Left (Client.ErrInvalidInput e) -> print e
--             Left (Client.ErrMalformedJSON e) -> print e
--             Left (Client.ErrInternalServerError e) -> print e
--             Left (Client.ErrUnauthorized e) -> print e
--             Left (Client.ErrUnknown e) -> print e

-- handlerJuristictionError :: JurisdictionError -> IO ()
-- handlerJuristictionError err = case err of
--       JurisdictionNotFound _ -> print "JurisdictionNotFound"
--       JurisdictionNameNotFound _ -> print "JurisdictionNameNotFound"
--       JurisdictionAlreadyExist _ -> print "JurisdictionAlreadyExist"
--       UnknownError -> print "UnknownError"