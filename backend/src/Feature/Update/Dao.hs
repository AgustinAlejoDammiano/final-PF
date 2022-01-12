module Feature.Update.Dao where

import ClassyPrelude
import Feature.Update.Types
import Feature.Common.Types
import Platform.Database
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import Data.Either
import Data.Time.LocalTime

listUpdatesFromDB :: Database r m => Pagination -> m [UpdateDate]
listUpdatesFromDB pagination = do
    result <- withConn handler $ \conn -> query conn qry (paginationLimit pagination, paginationOffset pagination)
    return $ fromRight [] result
    where qry = "select last_update from information limit greatest(0, ?) offset greatest(0, ?)"

createUpdateFromDB :: Database r m => ZonedTime -> m (Either UpdateError Update)
createUpdateFromDB param = do
    result <- withConn handler $ \conn -> execute conn qry (Only param)
    return $ second (\_ -> Update True) result
    where qry = "insert into information (last_update) values (?)"

handler :: (SqlError -> ConstraintViolation -> IO (Either UpdateError a))
handler _ _ = return $ Left UnknownError
