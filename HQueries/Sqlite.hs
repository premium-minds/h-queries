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
import Control.Monad

import qualified Data.Text.Encoding as TE

data SqliteBackend = SqliteBackend Sqlite.Connection

createSqliteBackend :: FilePath -> IO SqliteBackend
createSqliteBackend s = SqliteBackend `fmap` Sqlite.connectSqlite3 s

instance QBackend SqliteBackend where
    getBackendCode _ q = T.intercalate ";\n" $ reverse $ hqio2Sql q
    
    hQuery b@(SqliteBackend conn) q = do
        let stms = map T.unpack $ hqio2Sql q
        forM_ (reverse $ tail stms) $ \x -> do
            st <- prepare conn x
            executeRaw st
        res <-  quickQuery' conn (head stms) []
        commit conn
        return $ fst $ parseQueryRes $ sqlValue2QueryRawRes res

    migrateSchema b@(SqliteBackend conn) l = do
        forM_ l $ \(EntityObj x) -> do
            let (create, init) = entity2migration x
            let sql = create `T.append` init
            tInfo <- quickQuery' conn "SELECT sql FROM sqlite_master WHERE type='table' and name = ?" [SqlString $ T.unpack $ entityGetBackendName x]
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


