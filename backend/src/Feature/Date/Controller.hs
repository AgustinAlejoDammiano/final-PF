module Feature.Date.Controller(routes, Service(..)) where

import ClassyPrelude hiding (delete)

import Feature.Date.Types
import Feature.Common.Types
import Feature.Common.HTTP
import Web.Scotty.Trans

class Monad m => Service m where
    listDatesDose :: Pagination -> m [DateDose]

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do

    get "/api/date/dose" $ do
        pagination <- parsePagination
        result <- lift $ listDatesDose pagination
        json $ DatesWrapper result (length result)
