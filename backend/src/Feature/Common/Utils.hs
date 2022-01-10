module Feature.Common.Utils where

import ClassyPrelude
import Data.Text.Read (signed, decimal)


parseInt :: Text -> Int 
parseInt = either (error . show) fst . signed decimal 

parseIntger :: Text -> Integer
parseIntger = toInteger . parseInt
