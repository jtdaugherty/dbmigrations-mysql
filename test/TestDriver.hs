{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Database.Schema.Migrations.Backend.MySQL
import Database.Schema.Migrations.Test.BackendTest as BackendTest

import Control.Exception (catch, catches, finally, try, Handler(..), SomeException(..) )
import Data.String (fromString)
import qualified Database.MySQL.Base as MySQLBase
import qualified Database.MySQL.Simple as MySQLSimple
import System.Exit
import System.IO ( stderr )
import Test.HUnit

data MySQLBackendConnection = MySQLConnection MySQLSimple.Connection

instance BackendConnection MySQLBackendConnection where
    supportsTransactionalDDL = const False
    makeBackend (MySQLConnection c) = mysqlBackend c
    commit (MySQLConnection c) = MySQLSimple.commit c
    withTransaction (MySQLConnection c) transaction =
        MySQLSimple.withTransaction c (transaction (MySQLConnection c))
    getTables (MySQLConnection c) =
        fmap (map MySQLSimple.fromOnly)
            (MySQLSimple.query_ c "SHOW TABLES")
    catchAll (MySQLConnection _) act handler =
        act `catches`
            [ Handler (\(_ :: MySQLSimple.FormatError) -> handler)
            , Handler (\(_ :: MySQLSimple.QueryError) -> handler)
            , Handler (\(_ :: MySQLBase.MySQLError) -> handler)
            , Handler (\(_ :: MySQLSimple.ResultError) -> handler)
            ]

loadTests :: IO [Test]
loadTests = do

  mysqlConn <- setupMySQLDb

  let backendConnection :: MySQLBackendConnection = MySQLConnection mysqlConn

      testAct =  (BackendTest.tests backendConnection)
                 `finally`
                 (MySQLSimple.close mysqlConn >> teardownMySQLDb)

  return [ ("MySQL backend tests") ~: test testAct ]

tempDatabase :: String
tempDatabase = "dbmigrations_test"

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

setupMySQLDb :: IO MySQLSimple.Connection
setupMySQLDb = do
  teardownMySQLDb `catch` ignoreException
  conn <- MySQLSimple.connect MySQLSimple.defaultConnectInfo
  MySQLSimple.execute_ conn (fromString ("CREATE DATABASE " ++ tempDatabase))
  MySQLSimple.execute_ conn (fromString ("USE " ++ tempDatabase))
  pure conn

teardownMySQLDb :: IO ()
teardownMySQLDb = do
  conn <- MySQLSimple.connect MySQLSimple.defaultConnectInfo
  e <- try (MySQLSimple.execute_ conn (fromString ("DROP DATABASE " ++ tempDatabase)))
  case e of
    Left ex@SomeException{} -> error ("Failed to drop test MySQL database: " ++ show ex)
    Right _ -> return ()

main :: IO ()
main = do
  tests_ <- loadTests
  (testResults, _) <- runTestText (putTextToHandle stderr False) $ test tests_
  if errors testResults + failures testResults > 0
    then exitFailure
    else exitSuccess
