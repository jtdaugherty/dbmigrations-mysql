module Main
    ( main
    )
where

import Prelude  hiding (lookup)
import System.Environment (getArgs)
import System.Exit

import Database.Schema.Migrations.Backend.MySQL
import Moo.Core
import Moo.Main

main :: IO ()
main = do
  args <- getArgs
  (_, opts, _) <- procArgs args
  loadedConf <- loadConfiguration $ _configFilePath opts
  case loadedConf of
    Left e -> putStrLn e >> exitFailure
    Right conf -> do
      let connectionString = _connectionString conf
      connection <- connectMySQL connectionString
      let backend = mysqlBackend connection
          parameters = makeParameters conf backend
      mainWithParameters args parameters

