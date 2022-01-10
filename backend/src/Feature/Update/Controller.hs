module Feature.Update.Controller(routes, Service(..)) where

import ClassyPrelude hiding (delete)

import Feature.Update.Types
import Feature.Common.HTTP
import Web.Scotty.Trans
import Network.HTTP.Types.Status
-- import qualified Text.Digestive.Form as DF
-- import Text.Digestive.Form ((.:))

class Monad m => Service m where
    update :: Text -> m(Either UpdateError Update)

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do

    post "/api/update" $ do
        -- change url
        result <- stopIfError updateErrorHandler $ update "https://drive.google.com/uc?id=1O7jO75EU22h5Z58KxqM2UNEvp8zR9okp&export=download"
        json $ UpdateWrapper result

updateErrorHandler :: (ScottyError e, Monad m) => UpdateError -> ActionT e m ()
updateErrorHandler err = case err of
    URLNotFound _ -> do
        status status502
        json err
