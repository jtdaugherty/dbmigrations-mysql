module Main where
import Test.HUnit
import System.Exit
import System.IO ( stderr )

import qualified BackendTest

import Control.Exception ( finally, catch, try, SomeException(..) )
import Data.String (fromString)

import qualified Database.MySQL.Simple as MySQL

loadTests :: IO [Test]
loadTests = do

  mysqlConn <- setupMySQLDb

  let testAct =  (BackendTest.tests (BackendTest.MySQLConnection mysqlConn))
                 `finally`
                 (MySQL.close mysqlConn >> teardownMySQLDb)

  return [ ("MySQL backend tests") ~: test testAct ]

tempDatabase :: String
tempDatabase = "dbmigrations_test"

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

setupMySQLDb :: IO MySQL.Connection
setupMySQLDb = do
  teardownMySQLDb `catch` ignoreException
  conn <- MySQL.connect MySQL.defaultConnectInfo
  MySQL.execute_ conn (fromString ("CREATE DATABASE " ++ tempDatabase))
  MySQL.execute_ conn (fromString ("USE " ++ tempDatabase))
  pure conn

teardownMySQLDb :: IO ()
teardownMySQLDb = do
  conn <- MySQL.connect MySQL.defaultConnectInfo
  e <- try (MySQL.execute_ conn (fromString ("DROP DATABASE " ++ tempDatabase)))
  case e of
    Left ex@SomeException{} -> error ("Failed to drop test MySQL database: " ++ show ex)
    Right _ -> return ()

main :: IO ()
main = do
  tests <- loadTests
  (testResults, _) <- runTestText (putTextToHandle stderr False) $ test tests
  if errors testResults + failures testResults > 0
    then exitFailure
    else exitSuccess
