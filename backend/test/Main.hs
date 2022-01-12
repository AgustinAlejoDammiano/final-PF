import ClassyPrelude
import Test.Hspec
import System.Environment
import Database.PostgreSQL.Simple
import qualified Utils.Client as Client
import qualified Lib
import Control.Concurrent

import qualified Spec.Jurisdiction as Jurisdiction

main :: IO ()
main = withEnv . hspec $ do
  Jurisdiction.spec

withEnv :: IO () -> IO ()
withEnv = bracket startEnv cleanEnv . const

startEnv :: IO ThreadId
startEnv = do
    execPGQuery ["drop database if exists covid_test", "create database covid_test"]
    setEnv "DATABASE_URL" "postgresql://postgres:postgres@localhost/covid_test"
    setEnv "DATA_URL" "https://drive.google.com/uc?id=1O7jO75EU22h5Z58KxqM2UNEvp8zR9okp&export=download"
    setEnv "PORT" "3000"
    tId <- forkIO Lib.main
    unlessM healthCheck $ do
        putStrLn "Waiting for server ..."
        threadDelay 1000000
    return tId
    where
        healthCheck = either (const False) id <$> Client.runClient Client.health

cleanEnv :: ThreadId -> IO ()
cleanEnv tId = do
    killThread tId
    putStrLn $ "Sever killed (" <> tshow tId <> ")"

execPGQuery :: [Query] -> IO ()
execPGQuery qrys =
    bracket acquire release execQuery
    where
        --   TODO env varible for user and password
        acquire = connectPostgreSQL "postgresql://postgres:postgres@localhost"
        release = close
        execQuery conn = forM_ qrys (void . execute_ conn)
