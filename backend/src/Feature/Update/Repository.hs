module Feature.Update.Repository where

import ClassyPrelude
import Feature.Update.Types
import Feature.Jurisdiction.Types hiding (UnknownError)
import Feature.Common.Utils

import Control.Monad.Trans.Resource
import Data.Conduit.Combinators     (sinkFile)
import Network.HTTP.Conduit         
import Network.HTTP.Simple          (httpSink)
import Codec.Archive.Zip (withArchive, unpackInto)
import System.Directory
-- import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy as BL
import Data.Csv.Streaming
import Data.Csv (FromNamedRecord, parseNamedRecord, (.:))
import Data.Foldable as F

data Row = Row{ rowSex :: Text, rowJurisdiction :: Text, rowJurisdictionId :: Text} deriving (Eq, Show)

instance FromNamedRecord Row where
    parseNamedRecord x = Row <$> (x .: "sexo") <*> (x .: "jurisdiccion_residencia") <*> (x .: "jurisdiccion_residencia_id")

loadData :: (MonadThrow m, MonadUnliftIO m) => Text -> (CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)) -> m(Either UpdateError Update)
loadData url cj = do
    request <- parseRequest $ unpack url
    runResourceT $ httpSink request $ \_ -> sinkFile fileZip
    withArchive fileZip (unpackInto "./")
    result <- readCsv cj
    liftIO $ removeFile fileZip
    return result
    where fileZip = "tmpfile.zip"

readCsv :: (MonadIO m) => (CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)) -> m(Either UpdateError Update)
readCsv cj = do
    f <- liftIO $ BL.readFile fileCsv
    case decodeByName f of
        Left _ -> return $ Left $ UnknownError
        Right (_, xs) -> do
            F.for_ xs $ handleRow cj
            liftIO $ removeFile fileCsv
            return $ Right $ Update True
    where fileCsv = "datos_nomivac_covid19.csv"

handleRow :: (MonadIO m) => (CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)) -> Row -> m Bool
handleRow cj r = do
    _ <- cj $ CreateJurisdiction (parseIntger $ rowJurisdictionId r) (rowJurisdiction r)
    return True
