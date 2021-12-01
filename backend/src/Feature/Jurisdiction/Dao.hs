module Feature.Jurisdiction.Dao where

import ClassyPrelude
import Feature.Jurisdiction.Types
import Feature.Common.Types
import Platform.Database
import Database.PostgreSQL.Simple

listJurisdictionsFromDB :: Database r m => JurisdictionFilter -> Pagination -> m [Jurisdiction]
listJurisdictionsFromDB jf pagination = 
    withConn $ \conn -> query conn qry (jurisdictionFilterName jf, paginationLimit pagination, paginationOffset pagination)
    where qry = "select id, name from jurisdiction where coalesce(?, name) = name limit greatest(0, ?) offset greatest(0, ?)"

createJurisdictionFromDB :: Database r m => CreateJurisdiction -> m ()
createJurisdictionFromDB param = 
    void . withConn $ \conn -> execute conn qry (Only $ createJurisdictionName param)
    where qry = "insert into jurisdiction (name) values (?)"
    