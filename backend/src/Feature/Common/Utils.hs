module Feature.Common.Utils where

import ClassyPrelude
import Data.Text.Read (signed, decimal)

-- TODO check
parseInt :: Text -> Int 
parseInt = either (error . show) fst . signed decimal 

parseIntger :: Text -> Integer
parseIntger = toInteger . parseInt

parseDate :: Text -> Maybe Day
parseDate t = parseTimeM False defaultTimeLocale "%Y-%m-%d" $ unpack t

parseDateOrThrow :: Text -> Day
parseDateOrThrow t = case m of
    Just a -> a
    Nothing -> error "Date Error"
    where m = parseDate t
