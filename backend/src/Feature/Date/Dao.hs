module Feature.Date.Dao where

import ClassyPrelude
import Feature.Date.Types
import Feature.Common.Types
import Platform.Database
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import Data.Either

listDatesDoseFromDB :: Database r m => Pagination -> m [DateDose]
listDatesDoseFromDB pagination = do 
    result <- withConn handler $ \conn -> query conn qry (paginationLimit pagination, paginationOffset pagination)
    return $ fromRight [] result
    where qry = "\
    \with firstDose as ( \
        \select date, count(*) as total \
        \from dose_application join jurisdiction on dose_application.residence_jurisdiction_id = jurisdiction.id \
        \where serie = 1 and jurisdiction.name != 'S.I.' \
        \group by date \
    \), \
    \secondDose as ( \
        \select Date, count(*) as total \
        \from dose_application join jurisdiction on dose_application.residence_jurisdiction_id = jurisdiction.id \
        \where serie = 2 and jurisdiction.name != 'S.I.' \
        \group by date \
    \), \
    \thirdDose as ( \
        \select Date, count(*) as total \
        \from dose_application join jurisdiction on dose_application.residence_jurisdiction_id = jurisdiction.id \
        \where serie = 3 and jurisdiction.name != 'S.I.' \
        \group by date \
    \) \
    \select coalesce(coalesce(firstDose.date, secondDose.date), thirdDose.date), coalesce(firstDose.total, 0), coalesce(secondDose.total, 0), coalesce(thirdDose.total, 0), coalesce(firstDose.total + secondDose.total + thirdDose.total, 0) \
    \from firstDose full outer join secondDose on firstDose.date = secondDose.date full outer join thirdDose on firstDose.date = thirdDose.date \
    \limit greatest(0, ?) offset greatest(0, ?)"

handler :: (SqlError -> ConstraintViolation -> IO (Either DateError a))
handler _ _ = return $ Left UnknownError
