module Lib(main) where

import ClassyPrelude
import Control.Monad.Trans.Resource

import qualified Platform.Database as Database
import qualified Platform.Controller as MainController

import qualified Feature.Jurisdiction.Controller as JurisdictionController
import qualified Feature.Jurisdiction.Dao as JurisdictionDao
import qualified Feature.Jurisdiction.Service as JurisdictionService

import qualified Feature.Update.Controller as UpdateController
import qualified Feature.Update.Service as UpdateService
import qualified Feature.Update.Repository as UpdateRepository
import qualified Feature.Update.Dao as UpdateDao

main :: IO ()
main = do
    pgEnv <- Database.init
    let runner app = flip runReaderT pgEnv $ unAppT app
    MainController.main runner

type Env = (Database.Env)

newtype AppT a = AppT{ unAppT :: ReaderT Env IO a} deriving  ( Applicative, Functor, Monad, MonadIO, MonadReader Env, MonadThrow, MonadUnliftIO)

instance JurisdictionController.Service AppT where
    listJurisdictions = JurisdictionService.listJurisdictions
    getJurisdiction = JurisdictionService.getJurisdiction
    createJurisdiction = JurisdictionService.createJurisdiction
  
instance JurisdictionService.Dao AppT where
    listJurisdictionsFromDB = JurisdictionDao.listJurisdictionsFromDB
    createJurisdictionFromDB = JurisdictionDao.createJurisdictionFromDB

instance UpdateController.Service AppT where
    update = UpdateService.update
    listUpdates = UpdateService.listUpdates

instance UpdateService.JurisdictionService AppT where
    createJurisdiction = JurisdictionService.createJurisdiction

instance UpdateService.UpdateRepository AppT where
    loadData = UpdateRepository.loadData

instance UpdateService.UpdateDao AppT where
    listUpdatesFromDB = UpdateDao.listUpdatesFromDB
    createUpdateFromDB = UpdateDao.createUpdateFromDB
