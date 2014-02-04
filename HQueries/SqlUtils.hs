{-# LANGUAGE GADTs, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable#-}

module HQueries.SqlUtils(
      hqio2Sql
    , sqlValue2QueryRawRes
    , entity2migration
    , initSqlVarsTable
    , getSessionSql
    , freeSession
)where

import HQueries.Internal
import HQueries.Queries
import Database.HDBC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import Data.Generics.Schemes
import Data.Generics.Aliases
import Data.Data (Typeable,Data)
import Control.Monad.State

mainVarsTable :: Text
mainVarsTable = "h_queries_main_vars_table"

sessionsTrackTable :: Text
sessionsTrackTable = "h_queries_sessions_track_table"

replaceAux find replace x = if x == find then replace else x
replaceExpr find replace expr = everywhere (mkT $ replaceAux find replace) expr

data SqlVal = SqlTextLit Text
            | SqlIntLit Integer
            | SqlStmVal SqlStatement
            | SqlValPlus SqlVal SqlVal
            | SqlLastInsertedId
            | SqlValNull
            deriving (Data, Typeable, Eq)

sqlVal2Text :: Int -> SqlVal -> Text
sqlVal2Text s (SqlTextLit t) = T.concat ["'", T.replace "'" "''" t, "'"]
sqlVal2Text s (SqlIntLit i) = T.pack $ show i
sqlVal2Text s (SqlStmVal stm) = T.concat ["(", sql2Text s stm, ")"]
sqlVal2Text s (SqlValPlus x y) = T.concat [sqlVal2Text s x, " + ", sqlVal2Text s y]
sqlVal2Text s SqlLastInsertedId = "last_insert_rowid()"
sqlVal2Text s SqlValNull = "null"

data SqlStatement = SelectS [SqlVal]
                  | SelectSVars [Int]
                  | UnionAll [SqlStatement]
                  | SelectTable Text
                  | SelectCols [Int] SqlStatement
                  | SqlInsert Text SqlStatement
                  | SqlInsertPartial Text [Int] SqlStatement
                  | SqlStmVar Int
                  | SqlStmWithSub [Int] SqlStatement SqlStatement
                  | SqlStmCreateVar Int SqlStatement
                  deriving (Data, Typeable, Eq)
                  

sql2Text :: Int -> SqlStatement -> Text
sql2Text s (SelectS l) = T.concat [ "SELECT "
                                , T.intercalate 
                                    ", " 
                                    (map (\(v,i) -> T.concat [sqlVal2Text s v, " as ", mkName i]) (l `zip` [1..]))]
sql2Text s (UnionAll l) = T.intercalate " UNION ALL " (map (sql2Text s) l)
sql2Text s (SelectTable n) = T.concat ["SELECT * FROM ", n]
sql2Text s (SqlInsert n v) = T.concat ["INSERT INTO ", n, " ", sql2Text s v]
sql2Text s (SqlInsertPartial n c v) = T.concat ["INSERT INTO ", n, "(", T.intercalate "," (map mkName c), ") ", sql2Text s v]
sql2Text s (SelectCols cols sub) = 
    T.concat ["SELECT ", T.intercalate 
                            ", " 
                            (map 
                                (\(c,i) -> T.concat[mkName c, " as ", mkName i]) 
                                (cols `zip` [1..]))
             , " FROM ( "
             , sql2Text s sub, ")" ]
sql2Text s (SqlStmWithSub l p1 p2) = 
    let
        val = sql2Text s p1
    in
        T.concat [ "SELECT "
                 , T.intercalate 
                    ", " 
                    (map 
                        (\(c,i) -> T.concat ["(SELECT ", mkName c, " FROM (",val,")) as ", mkName i]) 
                        (l `zip` [1..])) 
                 , " from (", sql2Text s p2, ")"]
sql2Text s (SelectSVars l) = T.concat ["SELECT ", T.intercalate ", " (map mkName l)]
sql2Text s (SqlStmVar i) = T.concat ["SELECT val FROM ", mainVarsTable, " WHERE name = ", T.pack $ show i, " and session = ", T.pack $ show s ]
sql2Text s (SqlStmCreateVar i v) = 
    T.concat 
        [ "INSERT INTO ", mainVarsTable
        , " VALUES (", T.pack $ show s , ",", T.pack $ show i, ",", sqlVal2Text s (SqlStmVal v), ")" ]

data GenSqlState = GenSqlState{sqlStatements :: [SqlStatement]}
type GenSqlMonad = State GenSqlState

addSqlStatement :: SqlStatement -> GenSqlMonad ()
addSqlStatement x = do
    st <- get
    put $ st{sqlStatements = x:(sqlStatements st)}

runSql :: GenSqlMonad SqlStatement -> (SqlStatement, [SqlStatement])
runSql x =
    let
        (res, st) = runState x GenSqlState{sqlStatements = []}
    in
        (res, reverse $ sqlStatements st)

queryToSQL :: Query a -> GenSqlMonad SqlStatement
queryToSQL (ASTTextLit t) = return $ SelectS [SqlTextLit t]
queryToSQL (ASTProdTypeLit l) = do
    v <- mapM (\(QueryObj x) -> queryToSQL x) l
    return $ SelectS $ map SqlStmVal v
queryToSQL (ASTListLit l) = do
    lV <- mapM queryToSQL l
    return $ UnionAll lV
queryToSQL (ASTIntLit i) = return $ SelectS [SqlIntLit i]
queryToSQL (ASTPlusInt x y) = do
    xv <- queryToSQL x
    yv <- queryToSQL y
    return $ SelectS [SqlValPlus (SqlStmVal xv) (SqlStmVal $ yv)]
queryToSQL (ASTVar i) = return $ SqlStmVar i
queryToSQL r@(ASTQMap f lx) = do
    let QTypeRepList elemRep = queryGetQTypeRep lx
    let size = mkListColumnsCount elemRep
    let QTypeRepList elemRepRes = queryGetQTypeRep r
    let sizeRes = mkListColumnsCount elemRepRes
    vWithVar <- queryToSQL $ f (ASTVar $ -1)
    vL <- queryToSQL lx
    return $ SqlStmWithSub [1..sizeRes] (replaceExpr (SqlStmVar $ -1) (SelectSVars [1..size]) vWithVar) vL
queryToSQL (ASTGetEntity ref) = return $ SelectTable $ entityGetBackendName ref
queryToSQL (ASTInsertEntity x ref) = do
    vx <- queryToSQL x
    addSqlStatement $ SqlInsert (entityGetBackendName ref) vx
    return $ SelectS [SqlValNull]
queryToSQL (ASTDef i (ASTInsertEntityMapAK x ref)) = do
    let n = entityGetBackendName ref
    let nrCols = (mkListColumnsCount $ queryGetQTypeRep x) +1
    vx <- queryToSQL x
    addSqlStatement $ SqlInsertPartial n [2..nrCols] vx
    addSqlStatement $ SqlStmCreateVar i (SelectS [SqlLastInsertedId])
    queryToSQL $ ASTVar i
queryToSQL (ASTQValues m) = do
    let (QTypeRepMap keyRep valRep) = queryGetQTypeRep m
    let size = mkListColumnsCount valRep
    let start = mkListColumnsCount keyRep
    vm <- queryToSQL m
    return $ SelectCols [(start+1)..(start + size)] vm
queryToSQL (ASTProjection i s) = do
    vs <- queryToSQL s
    return $ SelectCols [i] vs

queryToSQL o = error $ "no clause for " ++ show o ++ " in SqlUtils\n"


genSql2Text :: Int -> GenSqlMonad SqlStatement -> (Text,[Text])
genSql2Text s x = 
    let
        (res, steps) = runSql x
    in 
        (sql2Text s res, map (sql2Text s) steps)

hqio2GenSqlMonad :: HQIO (Query a) -> GenSqlMonad SqlStatement
hqio2GenSqlMonad x = do
    let (res, steps) = runHQIO x
    forM_ steps $ \(QueryObj q) -> do
        queryToSQL q
    queryToSQL res

hqio2Sql :: Int -> HQIO (Query a) -> (Text, [Text])
hqio2Sql s x = 
    let
        m = hqio2GenSqlMonad x
    in
        genSql2Text s m


mkListColumns :: QTypeRep -> [Text]
mkListColumns (QTypeRepProd _ l) = concatMap mkListColumns l
mkListColumns (QTypeRepNewType x) = mkListColumns x
mkListColumns QTypeRepInt = ["INT"]
mkListColumns QTypeRepText = ["TEXT"]

mkListColumnsCount :: QTypeRep -> Int
mkListColumnsCount (QTypeRepProd _ l) = sum $ map mkListColumnsCount l
mkListColumnsCount (QTypeRepNewType x) = mkListColumnsCount x
mkListColumnsCount QTypeRepInt = 1
mkListColumnsCount QTypeRepText = 1
mkListColumnsCount o = error $ "can't make column list for " ++ show o

mkName :: Int -> Text
mkName i = T.pack $ "x" ++ show i

mkAllNames = (map mkName [1..])

entity2migration :: Entity a b c -> (Text,Text)
entity2migration x =
    let
        tpRep = entityGetQTypeRep x
        names = mkAllNames
        tname = entityGetBackendName x
        (cols,stms) = case tpRep of
            QTypeRepList l -> (names `zip` mkListColumns l, [])
            QTypeRepMap k v -> (names `zip` ("INTEGER PRIMARY KEY AUTOINCREMENT":(mkListColumns v)) , [])
            QTypeRepInt -> ([("x1","INT")], [T.concat["INSERT INTO ", tname, " VALUES (0)"]])
        pl = map (\(n, t) -> T.concat [n," ",t] ) $ cols
    in
        (T.concat[ "CREATE TABLE "
                , tname
                , " ("
                , T.intercalate "," pl
                , ");"]
        , T.intercalate ";" stms)


initSqlVarsTable :: [Text]
initSqlVarsTable = 
    [ T.concat ["CREATE TABLE IF NOT EXISTS ", mainVarsTable, " (session INTEGER, name INTEGER, val INTEGER)" ]
    , T.concat ["CREATE TABLE IF NOT EXISTS ", sessionsTrackTable, " (session INTEGER PRIMARY KEY, stamp timestamp)" ]]

getSessionSql :: (Text, Text)
getSessionSql = (T.concat["INSERT INTO ", sessionsTrackTable, "(stamp) VALUES (datetime('now'))"],"SELECT last_insert_rowid()")

freeSession :: Int -> [Text]
freeSession s =
    [ T.concat ["DELETE FROM ", mainVarsTable, " WHERE session = ", T.pack $ show s]
    , T.concat ["DELETE FROM ", sessionsTrackTable, " WHERE session = ", T.pack $ show s]]

sqlValue2QueryRawRes :: [[SqlValue]] -> QueryRawRes
sqlValue2QueryRawRes li =
    let
        l = concat li
        trans (SqlByteString bs) = bs
    in 
        QueryRawResSimple $ map trans l

