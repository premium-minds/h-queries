{-# LANGUAGE OverloadedStrings #-}
module HQueries.Sqlite (
    createSqliteBackend
) where

import HQueries.Internal
import HQueries.SqlUtils

import qualified Database.HDBC.Sqlite3 as Sqlite
import Database.HDBC

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as UTF8
import Control.Monad

import qualified Data.Text.Encoding as TE

data SqliteBackend = SqliteBackend Sqlite.Connection

createSqliteBackend :: FilePath -> IO SqliteBackend
createSqliteBackend s = do
    conn <- Sqlite.connectSqlite3 s
    executeListRaw conn initSqlVarsTable
    return $ SqliteBackend conn

executeListRaw :: IConnection c => c -> [Text] -> IO ()
executeListRaw conn l = do
    let stms = map T.unpack l
    forM_ stms $ \x -> do
        st <- prepare conn x
        executeRaw st

instance QBackend SqliteBackend where
    getBackendCode _ q =
        let 
            (res, steps) = hqio2Sql 0 q
        in
            T.intercalate ";\n" (steps ++ [res])
    
    hQuery b@(SqliteBackend conn) q = do
        let (sSql0, sSqlRes) = getSessionSql
        executeListRaw conn [sSql0]
        [[SqlByteString sRaw]] <- quickQuery' conn (T.unpack sSqlRes) []
        let s = read $ UTF8.unpack sRaw
        let (res, steps) = hqio2Sql s q
        executeListRaw conn steps
        res <-  quickQuery' conn (T.unpack res) []
        executeListRaw conn $ freeSession s
        commit conn
        return $ fst $ parseQueryRes $ sqlValue2QueryRawRes res

    migrateSchema b@(SqliteBackend conn) l = do
        forM_ l $ \(EntityObj x) -> do
            let (create, init) = entity2migration x
            let sql = create `T.append` init
            tInfo <- quickQuery' 
                        conn 
                        "SELECT sql FROM sqlite_master WHERE type='table' and name = ?" 
                        [SqlString $ T.unpack $ entityGetBackendName x]
            case tInfo of
                [] -> do
                    putStrLn $ show $ sql
                    runRaw conn (T.unpack sql)
                    commit conn
                [[SqlByteString bs]] -> do
                    if create == (TE.decodeUtf8 bs `T.append` ";") then
                        return ()
                        else
                            error $ T.unpack $ T.concat["table ", entityGetBackendName x, " with wrong schema"]


