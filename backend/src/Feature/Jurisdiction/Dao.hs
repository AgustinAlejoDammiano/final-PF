module Feature.Jurisdiction.Dao where

import ClassyPrelude
import Feature.Jurisdiction.Types
import Feature.Common.Types
import Platform.Database
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import Data.Either

listJurisdictionsFromDB :: Database r m => JurisdictionFilter -> Pagination -> m [Jurisdiction]
listJurisdictionsFromDB jf pagination = do
    result <- withConn handler $ \conn -> query conn qry (jurisdictionFilterName jf, paginationLimit pagination, paginationOffset pagination)
    return $ fromRight [] result
    where qry = "select id, name from jurisdiction where coalesce(?, name) = name limit greatest(0, ?) offset greatest(0, ?)"

createJurisdictionFromDB :: Database r m => CreateJurisdiction -> m (Either JurisdictionError Bool)
createJurisdictionFromDB param = do
    result <- withConn handler $ \conn -> execute conn qry (createJurisdictionId param, createJurisdictionName param)
    return $ second (\_ -> True) result
    where qry = "insert into jurisdiction (id, name) values (?, ?)"

deleteJurisdictionFromDB :: Database r m => Integer -> m (Either JurisdictionError Bool)
deleteJurisdictionFromDB i = do
    result <- withConn handler $ \conn -> execute conn qry (Only i)
    return $ second (\_ -> True) result
    where qry = "delete from jurisdiction where id = ?"

deleteJurisdictionsFromDB :: Database r m => m (Either JurisdictionError Bool)
deleteJurisdictionsFromDB = do
    result <- withConn handler $ \conn -> execute conn qry ()
    return $ second (\_ -> True) result
    where qry = "delete from jurisdiction"

handler :: (SqlError -> ConstraintViolation -> IO (Either JurisdictionError a))
handler _ (UniqueViolation x) = return $ Left $ JurisdictionAlreadyExist $ decodeUtf8 x
handler _ _ = return $ Left UnknownError
