module Feature.Update.Repository where

import ClassyPrelude
import Feature.Update.Types
import Feature.Jurisdiction.Types
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

-- TODO remove prints

loadData :: (MonadThrow m, MonadUnliftIO m) => Text -> (CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)) -> m(Either UpdateError Update)
loadData url cj = do
    -- TODO handle errors
    request <- parseRequest $ unpack url
    runResourceT $ httpSink request $ \_ -> sinkFile "tmpfile.zip"
    withArchive "tmpfile.zip" (unpackInto "./")
    result <- readCsv cj
    liftIO $ removeFile "tmpfile.zip"
    return result

readCsv :: (MonadIO m) => (CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)) -> m(Either UpdateError Update)
readCsv cj = do
    f <- liftIO $ BL.readFile "datos_nomivac_covid19.csv"
    case decodeByName f of
        -- TODO improve
        Left _ -> return $ Left $ URLNotFound "Cannot open CSV" 
        Right (_, xs) -> do
            F.for_ xs $ handleRow cj
            liftIO $ removeFile "datos_nomivac_covid19.csv"
            return $ Right $ Update True

handleRow :: (MonadIO m) => (CreateJurisdiction -> m(Either JurisdictionError Jurisdiction)) -> Row -> m Bool
handleRow cj r = do
    print (rowSex r, rowJurisdiction r, rowJurisdictionId r)
    result <- cj $ CreateJurisdiction (parseIntger $ rowJurisdictionId r) (rowJurisdiction r)
    -- TODO handle errors
    case result of
        Left _ -> print (rowJurisdictionId r)
        Right s -> print s
    return True
