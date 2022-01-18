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
    result <- withConn handler $ connection param
    return $ second (\_ -> True) result
    where 
        connection j
            | (createJurisdictionId j) >= 0 = \conn -> execute conn "insert into jurisdiction (id, name) values (?, ?)" (createJurisdictionId j, createJurisdictionName j)
            | otherwise = \conn -> execute conn "insert into jurisdiction (name) values (?)" (Only $ createJurisdictionName j)

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

listJurisdictionsDoseFromDB :: Database r m => Pagination -> m [JurisdictionDose]
listJurisdictionsDoseFromDB pagination = do 
    result <- withConn handler $ \conn -> query conn qry (paginationLimit pagination, paginationOffset pagination)
    return $ fromRight [] result
    where qry = "\
    \with firstDose as ( \
        \select residence_jurisdiction_id as id, count(*) as total \
        \from dose_application join jurisdiction on dose_application.residence_jurisdiction_id = jurisdiction.id \
        \where serie = 1 and jurisdiction.name != 'S.I.' \
        \group by residence_jurisdiction_id \
    \), \
    \secondDose as ( \
        \select residence_jurisdiction_id as id, count(*) as total \
        \from dose_application join jurisdiction on dose_application.residence_jurisdiction_id = jurisdiction.id \
        \where serie = 2 and jurisdiction.name != 'S.I.' \
        \group by residence_jurisdiction_id \
    \), \
    \thirdDose as ( \
        \select residence_jurisdiction_id as id, count(*) as total \
        \from dose_application join jurisdiction on dose_application.residence_jurisdiction_id = jurisdiction.id \
        \where serie = 3 and jurisdiction.name != 'S.I.' \
        \group by residence_jurisdiction_id \
    \) \
    \select jurisdiction.id, jurisdiction.name, firstDose.total, secondDose.total, thirdDose.total, firstDose.total + secondDose.total + thirdDose.total \
    \from firstDose join secondDose on firstDose.id = secondDose.id join thirdDose on firstDose.id = thirdDose.id join jurisdiction on firstDose.id = jurisdiction.id \
    \limit greatest(0, ?) offset greatest(0, ?)"

handler :: (SqlError -> ConstraintViolation -> IO (Either JurisdictionError a))
handler _ (UniqueViolation x) = return $ Left $ JurisdictionAlreadyExist $ decodeUtf8 x
handler _ _ = return $ Left UnknownError
