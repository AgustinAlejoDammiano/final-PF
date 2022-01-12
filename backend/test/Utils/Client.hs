module Utils.Client where

import ClassyPrelude
import Feature.Common.Types
import Feature.Jurisdiction.Types
import Spec.Types
import Network.Wreq hiding (Auth)
import Control.Monad.Except
import Control.Lens hiding ((.=))
import qualified Data.Aeson as Aeson
import Network.HTTP.Types.Status
import qualified Network.HTTP.Client as CLIENT
import Data.Has
import System.Environment

newtype Url = Url String

type Request r m = (MonadIO m, Has Url r, MonadReader r m, MonadUnliftIO m)

buildUrl :: (Request r m) => String -> ExceptT e m String
buildUrl path = do
    (Url baseUrl) <- asks getter
    return $ baseUrl <> path

health :: (Request r m) => m (Either (Err Text) Bool)
health = runExceptT $ do
    url <- buildUrl "/health"
    exec $ get url

createJurisdiction :: (Request r m) => CreateJurisdiction -> m (Either (Err JurisdictionError) Jurisdiction)
createJurisdiction arg = runExceptT $ do
    url <- buildUrl "/jurisdiction"
    let opts = defaults
    let body = Aeson.toJSON $ JurisdictionWrapper $ JurisdictionDto (pack $ show $ createJurisdictionId arg) (createJurisdictionName arg)
    jurisdictionWrapperJurisdiction <$> exec (postWith opts url body)

paginate :: Pagination -> Network.Wreq.Options -> Network.Wreq.Options
paginate (Pagination limit offset) =
    (param "limit" .~ [tshow limit]) . (param "offset" .~ [tshow offset])

exec :: (Request r m, Aeson.FromJSON a, Aeson.FromJSON e) => IO (Response LByteString) -> ExceptT (Err e) m a
exec req = ExceptT $ do
    r <- liftIO (Right <$> (req >>= asJSON))
        `catch` handleHttpException
        `catch` handleJSONError
        `catch` handleOtherException
    return $ case r of
        Left err -> Left err
        Right r' -> Right $ r' ^. responseBody
    where
        handleJSONError (JSONError err) = return . Left $ ErrMalformedJSON $ tshow err
        handleHttpException (CLIENT.HttpExceptionRequest _ (CLIENT.StatusCodeException res body)) =
            let status = CLIENT.responseStatus res
            in  if status == status500 then
                    return . Left $ ErrInternalServerError body
                else if status == status401 then
                    parseJSONError body ErrUnauthorized
                else if status == status422 then
                    parseJSONError body (ErrInvalidInput . errorsWrapperErrors)
                else
                    parseJSONError body ErrApp
        handleHttpException err = return . Left $ ErrUnknown $ tshow err
        handleOtherException (e :: SomeException) = return . Left $ ErrUnknown $ tshow e
        parseJSONError src f = case Aeson.eitherDecode $ fromStrict src of
            Left parseErr -> return . Left $ ErrMalformedJSON $ tshow parseErr
            Right parseResult -> return . Left $ f parseResult

runClient :: ReaderT Url m a -> m a
runClient = flip runReaderT (Url $"http://127.0.0.1:3000/api")
