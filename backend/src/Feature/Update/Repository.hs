module Feature.Update.Repository where

import ClassyPrelude
import Feature.Update.Types
import Feature.Jurisdiction.Types hiding (UnknownError)
import Feature.Department.Types hiding (UnknownError)
import Feature.Vaccine.Types hiding (UnknownError)
import Feature.Dose.Types hiding (UnknownError)
import Feature.Common.Utils

import Control.Monad.Trans.Resource
import Data.Conduit.Combinators     (sinkFile)
import Network.HTTP.Conduit         
import Network.HTTP.Simple          (httpSink)
import Codec.Archive.Zip (withArchive, unpackInto)
import System.Directory
import qualified Data.ByteString.Lazy as BL
import Data.Csv.Streaming
import Data.Csv (FromNamedRecord, parseNamedRecord, (.:))
import Data.Traversable as T
import Control.Monad.Except

data Row = Row{ rowJurisdictionResidence :: Text, rowJurisdictionResidenceId :: Text, 
                rowDepartmentResidence :: Text, rowDepartmentResidenceId :: Text, 
                rowVaccine :: Text, rowSex :: Text, rowAge :: Text, rowDate :: Text, rowSerie :: Integer, 
                rowCondition :: Text, rowLot :: Text, rowJurisdictionApplication :: Text, 
                rowJurisdictionApplicationId :: Text, rowDepartmentApplication :: Text, 
                rowDepartmentApplicationId :: Text } deriving (Eq, Show)

instance FromNamedRecord Row where
    parseNamedRecord x = Row <$> (x .: "jurisdiccion_residencia") <*> (x .: "jurisdiccion_residencia_id")
                        <*> (x .: "depto_residencia") <*> (x .: "depto_residencia_id") <*> (x .: "vacuna") 
                        <*> (x .: "sexo") <*> (x .: "grupo_etario") <*> (x .: "fecha_aplicacion")
                        <*> (x .: "orden_dosis") <*> (x .: "condicion_aplicacion") 
                        <*> (x .: "lote_vacuna") <*> (x .: "jurisdiccion_aplicacion") <*> (x .: "jurisdiccion_aplicacion_id")
                        <*> (x .: "depto_aplicacion") <*> (x .: "depto_aplicacion_id")
        
loadData :: (MonadThrow m, MonadUnliftIO m) => Text -> (CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)) -> (CreateDepartment -> m(Either DepartmentError Department)) -> (CreateVaccine -> m(Either VaccineError Vaccine)) -> (CreateDose -> m(Either DoseError Dose)) -> m(Either UpdateError Update)
loadData url cj cd cv cdo = do
    request <- parseRequest $ unpack url
    runResourceT $ httpSink request $ \_ -> sinkFile fileZip
    withArchive fileZip (unpackInto "./")
    result <- readCsv cj cd cv cdo
    liftIO $ removeFile fileZip
    return result
    where fileZip = "tmpfile.zip"

readCsv :: (MonadIO m) => (CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)) -> (CreateDepartment -> m(Either DepartmentError Department)) -> (CreateVaccine -> m(Either VaccineError Vaccine)) -> (CreateDose -> m(Either DoseError Dose)) -> m(Either UpdateError Update)
readCsv cj cd cv cdo = runExceptT $ do
    f <- withExceptT (\_ -> UnknownError) $ liftIO $ BL.readFile fileCsv
    case decodeByName f of
        Left _ -> throwError UnknownError
        Right (_, xs) -> do
            _ <- T.forM xs $ handleRow cj cd cv cdo
            liftIO $ removeFile fileCsv
            return $ Update True
    where fileCsv = "datos_nomivac_covid19.csv"

handleRow :: (MonadIO m) => (CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)) -> (CreateDepartment -> m(Either DepartmentError Department)) -> (CreateVaccine -> m(Either VaccineError Vaccine)) -> (CreateDose -> m(Either DoseError Dose)) -> Row -> ExceptT UpdateError m ()
handleRow cj cd cv cdo r = do
    _ <- customWithExceptT $ cj $ CreateJurisdiction (parseIntger $ rowJurisdictionResidenceId r) (rowJurisdictionResidence r)
    dr <- customWithExceptT $ cd $ CreateDepartment (-1) (rowDepartmentResidence r)    
    _ <- customWithExceptT $ cj $ CreateJurisdiction (parseIntger $ rowJurisdictionApplicationId r) (rowJurisdictionApplication r)
    da <- customWithExceptT $ cd $ CreateDepartment (-1) (rowDepartmentApplication r)
    v <- customWithExceptT $ cv $ CreateVaccine (rowVaccine r)
    _ <- customWithExceptT $ cdo $ CreateDose (rowSex r) (rowAge r) (rowCondition r) (rowLot r) (parseDateOrThrow $ rowDate r) (rowSerie r) (vaccineId v)
        (parseIntger $ rowJurisdictionResidenceId r) (departmentId dr) (parseIntger $ rowJurisdictionApplicationId r)
        (departmentId da)
    return ()
    where customWithExceptT a = withExceptT (\_ -> UnknownError) $ ExceptT a
