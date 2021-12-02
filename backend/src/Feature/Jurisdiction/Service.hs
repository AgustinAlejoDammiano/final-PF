module Feature.Jurisdiction.Service where

import ClassyPrelude
import Control.Monad.Except
import Feature.Jurisdiction.Types
import Feature.Common.Types

class (Monad m) => Dao m where
	listJurisdictionsFromDB ::  JurisdictionFilter -> Pagination -> m [Jurisdiction]
	createJurisdictionFromDB ::  CreateJurisdiction -> m()

listJurisdictions :: (Dao m) => JurisdictionFilter -> Pagination -> m [Jurisdiction]
listJurisdictions = listJurisdictionsFromDB

getJurisdiction :: (Dao m) => Text -> m(Either JurisdictionError Jurisdiction)
getJurisdiction name = runExceptT $ do
	result <- lift $ listJurisdictions (JurisdictionFilter $ Just name) (Pagination 1 0)
	case result of
		[jurisdiction] -> return jurisdiction
		_ -> throwError $ JurisdictionNameNotFound name

createJurisdiction :: (Dao m) => CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)
createJurisdiction param = do 
	createJurisdictionFromDB param
	getJurisdiction $ createJurisdictionName param