module Feature.Dose.Controller(routes, Service(..)) where

import ClassyPrelude hiding (delete)

import Feature.Dose.Types
import Feature.Common.Types
import Feature.Common.HTTP
import Feature.Common.Utils
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:), validate)
import Text.Digestive.Types

class Monad m => Service m where
    listDoses :: DoseFilter -> Pagination -> m [Dose]
    getDose :: Integer -> m(Either DoseError Dose)
    createDose :: CreateDose -> m(Either DoseError Dose)
    deleteDose :: Integer -> m (Either DoseError Bool)
    deleteDoses :: m (Either DoseError Bool)

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do

    get "/api/dose" $ do
        pagination <- parsePagination
        doseFilter <- parseDoseFilter
        result <- lift $ listDoses doseFilter pagination
        json $ DosesWrapper result (length result)

    post "/api/dose" $ do
        req <- parseJsonBody ("dose" .: createDoseForm)
        result <- stopIfError doseErrorHandler $ createDose req
        status status201
        json $ DoseWrapper result

    delete "/api/dose" $ do
        _ <- stopIfError doseErrorHandler $ deleteDoses 
        status status204

    delete "/api/dose/:id" $ do
        i <- param "id"
        _ <- stopIfError doseErrorHandler $ deleteDose i
        status status204

doseErrorHandler :: (ScottyError e, Monad m) => DoseError -> ActionT e m ()
doseErrorHandler err = case err of
    DoseNameNotFound _ -> do
        status status404
        json err
    DoseAlreadyExist _ -> do
        status status409
        json err
    UnknownError -> do
        status status500
        json err

-- TODO move to utils
mayParam :: (ScottyError e, Monad m) => LText -> ActionT e m (Maybe Text)
mayParam name = (Just <$> param name) `rescue` const (return Nothing)

mayParamInteger :: (ScottyError e, Monad m) => LText -> ActionT e m (Maybe Integer)
mayParamInteger name = do
    value <- mayParam name
    return $ maybe Nothing (\t -> readMay t) value

parseDoseFilter :: (ScottyError e, Monad m) => ActionT e m DoseFilter
parseDoseFilter = DoseFilter <$> mayParamInteger "id" <*> mayParam "sex"

createDoseForm :: (Monad m) => DF.Form [Text] m CreateDose
createDoseForm = CreateDose <$> "sex" .: DF.text Nothing <*> "age" .: DF.text Nothing  <*> "condition" .: DF.text Nothing
    <*> "lot" .: DF.text Nothing <*> "date" .: validate validateDate (DF.text Nothing) <*> "serie" .: DF.stringRead ["Not a number"] (Just 0)
    <*> "vaccineId" .: DF.stringRead ["Not a number"] (Just 0) <*> "residenceJurisdictionId" .: DF.stringRead ["Not a number"] (Just 0)
    <*> "residenceDepartmentId" .: DF.stringRead ["Not a number"] (Just 0) <*> "applicationJurisdictionId" .: DF.stringRead ["Not a number"] (Just 0)
    <*> "applicationDepartmentId" .: DF.stringRead ["Not a number"] (Just 0)

validateDate :: Text -> Result [Text] Day
validateDate t = maybe (Error ["Date must have the yyyy-mm-dd format"]) (\s -> Success s) (parseDate t)

-- createDoseSex :: Text, createDoseAge :: Text, createDoseCondition :: Text, 
--     createDoseLot :: Text, createDoseDate :: Day, createDoseSerie :: Integer, createDoseVaccineId :: Integer, 
--     createDoseResidenceJurisdictionId :: Integer, createDoseResidenceDepartmentId :: Integer, 
--     createDoseApplicationJurisdictionId :: Integer, createDoseApplicationDepartmentId :: Integer
