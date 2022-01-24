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
    deleteJurisdiction :: Integer -> m (Either JurisdictionError Bool)
    deleteJurisdictions :: m (Either JurisdictionError Bool)
    listJurisdictionsDose :: Pagination -> m [JurisdictionDose]

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
        status status201
        json $ JurisdictionWrapper result

    delete "/api/jurisdiction" $ do
        _ <- stopIfError jurisdictionErrorHandler $ deleteJurisdictions 
        status status204

    delete "/api/jurisdiction/:id" $ do
        i <- param "id"
        _ <- stopIfError jurisdictionErrorHandler $ deleteJurisdiction i
        status status204

    get "/api/jurisdiction/dose" $ do
        pagination <- parsePagination
        result <- lift $ listJurisdictionsDose pagination
        json $ JurisdictionsWrapper result (length result)

jurisdictionErrorHandler :: (ScottyError e, Monad m) => JurisdictionError -> ActionT e m ()
jurisdictionErrorHandler err = case err of
    JurisdictionNameNotFound _ -> do
        status status404
        json err
    JurisdictionAlreadyExist _ -> do
        status status409
        json err
    UnknownError -> do
        status status500
        json err

mayParam :: (ScottyError e, Monad m) => LText -> ActionT e m (Maybe Text)
mayParam name = (Just <$> param name) `rescue` const (return Nothing)

parseJurisdictionFilter :: (ScottyError e, Monad m) => ActionT e m JurisdictionFilter
parseJurisdictionFilter = JurisdictionFilter <$> mayParam "name"

createJurisdictionForm :: (Monad m) => DF.Form [Text] m CreateJurisdiction
createJurisdictionForm = CreateJurisdiction <$> "id" .: DF.stringRead ["Not a number"] (Just $ -1)
                                            <*> "name" .: DF.check ["Can't be empty"] (not . null) (DF.text Nothing)
