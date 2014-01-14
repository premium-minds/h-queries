module HQueries.Sqlite (
    createSqliteBackend
) where

import HQueries.Internal
import HQueries.SqlUtils

import qualified Database.HDBC.Sqlite3 as Sqlite
import Database.HDBC

import Data.Text (Text)
import qualified Data.Text as T

data SqliteBackend = SqliteBackend Sqlite.Connection

createSqliteBackend :: FilePath -> IO SqliteBackend
createSqliteBackend s = SqliteBackend `fmap` Sqlite.connectSqlite3 s

instance QBackend SqliteBackend where
    getBackendCode _ q = queryToSQL q
    hQuery b@(SqliteBackend conn) q = do
    res <- quickQuery' conn (T.unpack $ getBackendCode b q) []
    return $ fst $ parseQueryRes $ sqlValue2QueryRawRes res


