module Feature.Jurisdiction.Controller(routes, Service(..)) where

import ClassyPrelude hiding (delete)

import Feature.Jurisdiction.Types
import Feature.Common.Types
import Feature.Common.HTTP
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))

class Monad m => Service m where
    listJurisdictions :: JurisdictionFilter -> Pagination -> m [Jurisdiction]
    getJurisdiction :: Text -> m(Either JurisdictionError Jurisdiction)
    createJurisdiction :: CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do

    get "/api/jurisdiction" $ do
        pagination <- parsePagination
        jurisdictionFilter <- parseJurisdictionFilter
        result <- lift $ listJurisdictions jurisdictionFilter pagination
        json $ JurisdictionsWrapper result (length result)

    post "/api/jurisdiction" $ do
        req <- parseJsonBody ("jurisdiction" .: createJurisdictionForm)
        result <- stopIfError jurisdictionErrorHandler $ createJurisdiction req
        json $ JurisdictionWrapper result

jurisdictionErrorHandler :: (ScottyError e, Monad m) => JurisdictionError -> ActionT e m ()
jurisdictionErrorHandler err = case err of
  	JurisdictionNotFound _ -> do
		status status404
		json err
  	JurisdictionNameNotFound _ -> do
		status status404
		json err

mayParam :: (ScottyError e, Monad m) => LText -> ActionT e m (Maybe Text)
mayParam name = (Just <$> param name) `rescue` const (return Nothing)

parseJurisdictionFilter :: (ScottyError e, Monad m) => ActionT e m JurisdictionFilter
parseJurisdictionFilter = JurisdictionFilter <$> mayParam "name"

createJurisdictionForm :: (Monad m) => DF.Form [Text] m CreateJurisdiction
createJurisdictionForm = CreateJurisdiction <$> "name" .: DF.text Nothing