module Platform.Controller(main) where

import ClassyPrelude

import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Middleware.Cors

import qualified Feature.Jurisdiction.Controller as Jurisdiction

import System.Environment

type App r m = (Jurisdiction.Service m, MonadIO m)

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
	
	get "/api/health" $
		json True
