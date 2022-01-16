module Platform.Controller(main) where

import ClassyPrelude

import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Middleware.Cors

import qualified Feature.Jurisdiction.Controller as Jurisdiction
import qualified Feature.Department.Controller as Department
import qualified Feature.Vaccine.Controller as Vaccine
import qualified Feature.Dose.Controller as Dose
import qualified Feature.Date.Controller as Date
import qualified Feature.Update.Controller as Update

import System.Environment

type App r m = (Jurisdiction.Service m, Department.Service m, Vaccine.Service m, Dose.Service m, Date.Service m, Update.Service m, MonadIO m)

main :: (App r m) => (m Response -> IO Response) -> IO ()
main runner = do
      port <- acquirePort
      scottyT port runner routes
      where
        acquirePort = do
              port <- fromMaybe "" <$> lookupEnv "PORT"
              return . fromMaybe 3000 $ readMay port

routes :: (App r m) => ScottyT LText m ()
routes = do
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
        { corsRequestHeaders = "Authorization":simpleHeaders
        , corsMethods = "PUT":"DELETE":simpleMethods
        }
    options (regex ".*") $ return ()

    defaultHandler $ \str -> do
        status status500
        json str

    Jurisdiction.routes
    Department.routes
    Vaccine.routes
    Dose.routes
    Date.routes
    Update.routes

    get "/api/health" $
        json True
