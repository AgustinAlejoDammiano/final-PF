module Lib(main) where

import ClassyPrelude
import Control.Monad.Trans.Resource

import qualified Platform.Database as Database
import qualified Platform.Controller as MainController

import qualified Feature.Jurisdiction.Controller as JurisdictionController
import qualified Feature.Jurisdiction.Dao as JurisdictionDao
import qualified Feature.Jurisdiction.Service as JurisdictionService

import qualified Feature.Department.Controller as DepartmentController
import qualified Feature.Department.Dao as DepartmentDao
import qualified Feature.Department.Service as DepartmentService

import qualified Feature.Vaccine.Controller as VaccineController
import qualified Feature.Vaccine.Dao as VaccineDao
import qualified Feature.Vaccine.Service as VaccineService

import qualified Feature.Dose.Controller as DoseController
import qualified Feature.Dose.Dao as DoseDao
import qualified Feature.Dose.Service as DoseService

import qualified Feature.Update.Controller as UpdateController
import qualified Feature.Update.Service as UpdateService
import qualified Feature.Update.Repository as UpdateRepository
import qualified Feature.Update.Dao as UpdateDao

import qualified Feature.Date.Controller as DateController
import qualified Feature.Date.Dao as DateDao
import qualified Feature.Date.Service as DateService

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
    deleteJurisdiction = JurisdictionService.deleteJurisdiction
    deleteJurisdictions = JurisdictionService.deleteJurisdictions
    listJurisdictionsDose = JurisdictionService.listJurisdictionsDose

instance JurisdictionService.Dao AppT where
    listJurisdictionsFromDB = JurisdictionDao.listJurisdictionsFromDB
    createJurisdictionFromDB = JurisdictionDao.createJurisdictionFromDB
    deleteJurisdictionFromDB = JurisdictionDao.deleteJurisdictionFromDB
    deleteJurisdictionsFromDB = JurisdictionDao.deleteJurisdictionsFromDB
    listJurisdictionsDoseFromDB = JurisdictionDao.listJurisdictionsDoseFromDB

instance UpdateController.Service AppT where
    update = UpdateService.update
    listUpdates = UpdateService.listUpdates

instance UpdateService.JurisdictionService AppT where
    createJurisdiction = JurisdictionService.createJurisdiction
    deleteJurisdictions = JurisdictionService.deleteJurisdictions

instance UpdateService.DepartmentService AppT where
    createDepartment = DepartmentService.createDepartment
    deleteDepartments = DepartmentService.deleteDepartments

instance UpdateService.VaccineService AppT where
    createVaccine = VaccineService.createVaccineOrFind
    deleteVaccines = VaccineService.deleteVaccines

instance UpdateService.DoseService AppT where
    createDose = DoseService.createDose
    deleteDoses = DoseService.deleteDoses

instance UpdateService.UpdateRepository AppT where
    loadData = UpdateRepository.loadData

instance UpdateService.UpdateDao AppT where
    listUpdatesFromDB = UpdateDao.listUpdatesFromDB
    createUpdateFromDB = UpdateDao.createUpdateFromDB

instance DepartmentController.Service AppT where
    listDepartments = DepartmentService.listDepartments
    getDepartment = DepartmentService.getDepartment
    createDepartment = DepartmentService.createDepartment
    deleteDepartment = DepartmentService.deleteDepartment
    deleteDepartments = DepartmentService.deleteDepartments
  
instance DepartmentService.Dao AppT where
    listDepartmentsFromDB = DepartmentDao.listDepartmentsFromDB
    createDepartmentFromDB = DepartmentDao.createDepartmentFromDB
    deleteDepartmentFromDB = DepartmentDao.deleteDepartmentFromDB
    deleteDepartmentsFromDB = DepartmentDao.deleteDepartmentsFromDB

instance VaccineController.Service AppT where
    listVaccines = VaccineService.listVaccines
    getVaccine = VaccineService.getVaccine
    createVaccine = VaccineService.createVaccine
    deleteVaccine = VaccineService.deleteVaccine
    deleteVaccines = VaccineService.deleteVaccines
  
instance VaccineService.Dao AppT where
    listVaccinesFromDB = VaccineDao.listVaccinesFromDB
    createVaccineFromDB = VaccineDao.createVaccineFromDB
    deleteVaccineFromDB = VaccineDao.deleteVaccineFromDB
    deleteVaccinesFromDB = VaccineDao.deleteVaccinesFromDB

instance DoseController.Service AppT where
    listDoses = DoseService.listDoses
    getDose = DoseService.getDose
    createDose = DoseService.createDose
    deleteDose = DoseService.deleteDose
    deleteDoses = DoseService.deleteDoses
  
instance DoseService.Dao AppT where
    listDosesFromDB = DoseDao.listDosesFromDB
    createDoseFromDB = DoseDao.createDoseFromDB
    deleteDoseFromDB = DoseDao.deleteDoseFromDB
    deleteDosesFromDB = DoseDao.deleteDosesFromDB

instance DateController.Service AppT where
    listDatesDose = DateService.listDatesDose

instance DateService.Dao AppT where
    listDatesDoseFromDB = DateDao.listDatesDoseFromDB
