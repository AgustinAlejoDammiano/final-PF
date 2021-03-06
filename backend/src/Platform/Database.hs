module Platform.Database where

import ClassyPrelude
import Data.Has
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import Database.PostgreSQL.Simple.Migration
import System.Environment

type Env = Pool Connection

type Database r m = (MonadReader r m, Has Env r, MonadIO m)

init :: IO Env
init = do
    pool <- acquirePool
    migrateDb pool
    return pool

acquirePool :: IO (Pool Connection)
acquirePool = do
    envUrl <- lookupEnv "DATABASE_URL"
    let databaseUrl = fromString $ fromMaybe "postgresql://postgres:postgres@localhost:5432/covid" envUrl
    createPool (connectPostgreSQL databaseUrl) close 1 10 10

migrateDb :: Pool Connection -> IO ()
migrateDb pool = withResource pool $ \conn ->
    void $ withTransaction conn (runMigration (ctx conn))
    where
        ctx = MigrationContext cmd False
        cmd = MigrationCommands [ MigrationInitialization, MigrationDirectory "postgresql" ]

withConn :: Database r m => (SqlError -> ConstraintViolation -> IO (Either e a)) -> (Connection -> IO a) -> m (Either e a)
withConn catcher action = do
    pool <- asks getter
    liftIO $ catchViolation catcher (fmap Right (withResource pool action))
  