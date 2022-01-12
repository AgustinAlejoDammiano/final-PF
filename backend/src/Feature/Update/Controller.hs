module Feature.Update.Controller(routes, Service(..)) where

import ClassyPrelude hiding (delete)

import Feature.Update.Types
import Feature.Common.Types
import Feature.Common.HTTP
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import System.Environment

class Monad m => Service m where
    listUpdates :: Pagination -> m [UpdateDate]
    update :: Text -> m(Either UpdateError Update)

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do

    post "/api/update" $ do
        let defaultUrl = "https://sisa.msal.gov.ar/datos/descargas/covid-19/files/datos_nomivac_covid19.zip"
        url <- liftIO $ lookupEnv "DATA_URL"
        result <- stopIfError updateErrorHandler $ update $ pack $ fromMaybe defaultUrl url
        json $ UpdateWrapper result

    get "/api/update" $ do
        pagination <- parsePagination
        result <- lift $ listUpdates pagination
        json $ UpdatesWrapper result (length result)

updateErrorHandler :: (ScottyError e, Monad m) => UpdateError -> ActionT e m ()
updateErrorHandler err = case err of
    URLNotFound _ -> do
        status status502
        json err
    UnknownError -> do
        status status500
        json err
