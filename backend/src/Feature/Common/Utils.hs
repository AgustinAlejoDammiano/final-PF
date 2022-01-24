module Feature.Common.Utils where

import ClassyPrelude
import Data.Text.Read (signed, decimal)
import Data.Aeson.TH
import Language.Haskell.TH.Syntax

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

commonJSONDeriveMany :: [Name] -> Q [Dec]
commonJSONDeriveMany names =
      concat <$> mapM commonJSONDerive names

commonJSONDerive :: Name -> Q [Dec]
commonJSONDerive name =
    let lowerCaseFirst (y:ys) = toLower [y] <> ys 
        lowerCaseFirst "" = ""
        structName = fromMaybe "" . lastMay . splitElem '.' . show $ name
    in deriveJSON defaultOptions{fieldLabelModifier = lowerCaseFirst . drop (length structName)} name
