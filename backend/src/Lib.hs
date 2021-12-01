module Lib(main) where

import ClassyPrelude

import qualified Platform.Database as Database
import qualified Platform.Controller as MainController

import qualified Feature.Jurisdiction.Controller as JurisdictionController
import qualified Feature.Jurisdiction.Dao as JurisdictionDao
import qualified Feature.Jurisdiction.Service as JurisdictionService

main :: IO ()
main = do
  pgEnv <- Database.init
  let runner app = flip runReaderT pgEnv $ unAppT app
  MainController.main runner

type Env = (Database.Env)

newtype AppT a = AppT{ unAppT :: ReaderT Env IO a} deriving  ( Applicative, Functor, Monad, MonadIO, MonadReader Env)

instance JurisdictionController.Service AppT where
  listJurisdictions = JurisdictionService.listJurisdictions
  getJurisdiction = JurisdictionService.getJurisdiction
  createJurisdiction = JurisdictionService.createJurisdiction
  
instance JurisdictionService.Dao AppT where
  listJurisdictionsFromDB = JurisdictionDao.listJurisdictionsFromDB
  createJurisdictionFromDB = JurisdictionDao.createJurisdictionFromDB
