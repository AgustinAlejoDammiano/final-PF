module Feature.Vaccine.Controller(routes, Service(..)) where

import ClassyPrelude hiding (delete)

import Feature.Vaccine.Types
import Feature.Common.Types
import Feature.Common.HTTP
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))

class Monad m => Service m where
    listVaccines :: VaccineFilter -> Pagination -> m [Vaccine]
    getVaccine :: Text -> m(Either VaccineError Vaccine)
    createVaccine :: CreateVaccine -> m(Either VaccineError Vaccine)
    deleteVaccine :: Integer -> m (Either VaccineError Bool)
    deleteVaccines :: m (Either VaccineError Bool)

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do

    get "/api/vaccine" $ do
        pagination <- parsePagination
        vaccineFilter <- parseVaccineFilter
        result <- lift $ listVaccines vaccineFilter pagination
        json $ VaccinesWrapper result (length result)

    post "/api/vaccine" $ do
        req <- parseJsonBody ("vaccine" .: createVaccineForm)
        result <- stopIfError vaccineErrorHandler $ createVaccine req
        status status201
        json $ VaccineWrapper result

    delete "/api/vaccine" $ do
        _ <- stopIfError vaccineErrorHandler $ deleteVaccines 
        status status204

    delete "/api/vaccine/:id" $ do
        i <- param "id"
        _ <- stopIfError vaccineErrorHandler $ deleteVaccine i
        status status204

vaccineErrorHandler :: (ScottyError e, Monad m) => VaccineError -> ActionT e m ()
vaccineErrorHandler err = case err of
    VaccineNotFound _ -> do
        status status404
        json err
    VaccineNameNotFound _ -> do
        status status404
        json err
    VaccineAlreadyExist _ -> do
        status status409
        json err
    UnknownError -> do
        status status500
        json err

mayParam :: (ScottyError e, Monad m) => LText -> ActionT e m (Maybe Text)
mayParam name = (Just <$> param name) `rescue` const (return Nothing)

parseVaccineFilter :: (ScottyError e, Monad m) => ActionT e m VaccineFilter
parseVaccineFilter = VaccineFilter <$> mayParam "name"

createVaccineForm :: (Monad m) => DF.Form [Text] m CreateVaccine
createVaccineForm = CreateVaccine <$> "name" .: DF.text Nothing
