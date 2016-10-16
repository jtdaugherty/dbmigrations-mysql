module Main
    ( main
    )
where

import Prelude  hiding (lookup)
import System.Environment (getArgs, setEnv)
import System.Exit

import Database.Schema.Migrations.Backend.MySQL
import Moo.Core
import Moo.Main

main :: IO ()
main = do
  -- make sure the configuration validation does not complain about a missing
  -- db type
  setEnv "DBM_DATABASE_TYPE" "mysql"

  args <- getArgs
  (_, opts, _) <- procArgs args
  conf <-
    loadConfiguration $ _configFilePath opts
  case conf of
      Left e -> putStrLn e >> exitFailure
      Right preliminaryConf -> do
        let Left preliminaryDatabaseConf =  _backendOrConf preliminaryConf
            connectionString = _connectionString preliminaryDatabaseConf
        connection <- connectMySQL connectionString
        let backend = mysqlBackend connection
            finalConf = preliminaryConf { _backendOrConf = Right backend }
        mainWithConf args finalConf

