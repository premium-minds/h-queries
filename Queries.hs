{-# LANGUAGE GADTs, RankNTypes #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Queries (
    Query(..)
    ,toQuery
    ,hQuery
    ,createSqliteBackend
    ,getBackendCode
    ,qmap
    ,queryEntity
    --, QInt
    ,Entity(..)
) where 
    import Data.List
    import qualified Database.HDBC.Sqlite3 as Sqlite
    import Database.HDBC
    import qualified Data.ByteString.Char8 as UTF8
    import qualified Data.ByteString as BS
    

    data Query a where
        ASTIntLit :: Integer -> Query Integer
        ASTListLit :: forall b. QType b => [b] -> Query [b]
        ASTQMap :: (Query a -> Query b) -> Query [a] -> Query [b]
        ASTVar :: Query b
        ASTPlusInt :: Query Integer -> Query Integer -> Query Integer
        ASTGetEntity :: Entity b -> Query b

    data Entity a = Entity String deriving (Show)

    queryEntity :: Entity a -> Query a
    queryEntity e = ASTGetEntity e

    queryToSQL :: Query a -> String
    queryToSQL (ASTIntLit i) = "select " ++ show i
    queryToSQL (ASTPlusInt x y) = "select (" ++ queryToSQL x ++ ")+(" ++ queryToSQL y ++ ")"
    queryToSQL ASTVar = "x1"
    queryToSQL (ASTGetEntity (Entity ref)) = "select * from " ++ ref 
    queryToSQL (ASTQMap f lx) = "select (" ++ (queryToSQL $ f ASTVar) ++ ") from (" ++ queryToSQL lx ++ ")"

    data QueryRawRes = QueryRawResSimple [BS.ByteString] deriving Show

    class QType a where
        toQuery :: a -> (Query a)
        parseQueryRes :: QueryRawRes -> (a, QueryRawRes)

    instance QType Integer where
        toQuery i = ASTIntLit i
        parseQueryRes (QueryRawResSimple (bs:r)) = (read $ UTF8.unpack bs, QueryRawResSimple r)

    instance QType a => QType [a] where
        toQuery l = ASTListLit l 
        parseQueryRes qr = (collectListRes parseQueryRes qr, QueryRawResSimple [])


    collectListRes :: (QueryRawRes -> (a, QueryRawRes)) -> QueryRawRes -> [a]
    collectListRes f (QueryRawResSimple []) = []
    collectListRes f l = let (t, r) = f l in [t] ++ collectListRes f r



    qmap :: (Query a -> Query b) -> Query [a] -> Query [b]
    qmap f lx = ASTQMap f lx

    instance Num (Query Integer) where
        x + y = ASTPlusInt x y

    class QBackend a where
        hQuery :: QType b => a -> Query b -> IO b
        getBackendCode :: QType b => a -> Query b -> String

    
    data SqliteBackend = SqliteBackend Sqlite.Connection

    createSqliteBackend :: FilePath -> IO SqliteBackend
    createSqliteBackend s = SqliteBackend `fmap` Sqlite.connectSqlite3 s

    sqlValue2QueryRawRes :: [[SqlValue]] -> QueryRawRes
    sqlValue2QueryRawRes li =
        let
            l = concat li
            trans (SqlByteString bs) = bs
        in 
            QueryRawResSimple $ map trans l

    instance QBackend SqliteBackend where
        getBackendCode _ q = queryToSQL q
        hQuery b@(SqliteBackend conn) q = do
        res <- quickQuery' conn (getBackendCode b q) []
        putStrLn $ show $ sqlValue2QueryRawRes res
        return $ fst $ parseQueryRes $ sqlValue2QueryRawRes res

    
