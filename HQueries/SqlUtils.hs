{-# LANGUAGE GADTs, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable#-}

module HQueries.SqlUtils(
     hqio2Sql
    ,sqlValue2QueryRawRes
    ,entity2migration
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

replaceAux find replace x = if x == find then replace else x
replaceExpr find replace expr = everywhere (mkT $ replaceAux find replace) expr

data SqlVal = SqlTextLit Text
            | SqlIntLit Integer
            | SqlStmVal SqlStatement
            | SqlValPlus SqlVal SqlVal
            | SqlLastInsertedId
            deriving (Data, Typeable, Eq)

sqlVal2Text :: SqlVal -> Text
sqlVal2Text (SqlTextLit t) = T.concat ["'", T.replace "'" "''" t, "'"]
sqlVal2Text (SqlIntLit i) = T.pack $ show i
sqlVal2Text (SqlStmVal s) = T.concat ["(", sql2Text s, ")"]
sqlVal2Text (SqlValPlus x y) = T.concat [sqlVal2Text x, " + ", sqlVal2Text y]
sqlVal2Text SqlLastInsertedId = "last_insert_rowid()"

data SqlStatement = SelectS [SqlVal]
                  | SelectSVars [Int]
                  | UnionAll [SqlStatement]
                  | SelectTable Text
                  | SelectCols [Int] SqlStatement
                  | SqlInsert Text SqlStatement
                  | SqlInsertPartial Text [Int] SqlStatement
                  | SqlStmVar
                  | SqlStmWithSub [Int] SqlStatement SqlStatement
                  deriving (Data, Typeable, Eq)
                  

sql2Text :: SqlStatement -> Text
sql2Text (SelectS l) = T.concat ["SELECT ", T.intercalate ", " (map (\(v,i) -> T.concat [sqlVal2Text v, " as ", mkName i]) (l `zip` [1..]))]
sql2Text (UnionAll l) = T.intercalate " UNION ALL " (map sql2Text l)
sql2Text (SelectTable n) = T.concat ["SELECT * FROM ", n]
sql2Text (SqlInsert n v) = T.concat ["INSERT INTO ", n, " ", sql2Text v]
sql2Text (SqlInsertPartial n c v) = T.concat ["INSERT INTO ", n, "(", T.intercalate "," (map mkName c), ") ", sql2Text v]
sql2Text (SelectCols cols sub) = 
    T.concat ["SELECT ", T.intercalate ", " (map (\(c,i) -> T.concat[mkName c, " as ", mkName i]) (cols `zip` [1..]))
             , " FROM ( "
             , sql2Text sub, ")" ]
sql2Text (SqlStmWithSub l p1 p2) = 
    let
        val = sql2Text p1
    in
        T.concat [ "SELECT "
                 , T.intercalate ", " (map (\(c,i) -> T.concat ["(SELECT ", mkName c, " FROM (",val,")) as ", mkName i]) (l `zip` [1..])) 
                 , " from (", sql2Text p2, ")"]
sql2Text (SelectSVars l) = T.concat ["SELECT ", T.intercalate ", " (map mkName l)]
sql2Text SqlStmVar = "Var"


queryToSQL :: Query a -> [SqlStatement]
queryToSQL (ASTTextLit t) = [SelectS [SqlTextLit t]]
queryToSQL (ASTProdTypeLit l) = [SelectS $ map (\(QTypeObj x) -> SqlStmVal $ head $ queryToSQL $ toQuery x) l]
queryToSQL (ASTListLit l) = [UnionAll ((head . queryToSQL . toQuery) `map` l ) ]
queryToSQL (ASTIntLit i) = [SelectS [SqlIntLit i]]
queryToSQL (ASTPlusInt x y) = [SelectS [SqlValPlus (SqlStmVal $ head $ queryToSQL x) (SqlStmVal $ head $ queryToSQL y)]]
queryToSQL ASTVar = [SqlStmVar]
queryToSQL r@(ASTQMap f lx) = 
    let
        QTypeRepList elemRep = queryGetQTypeRep lx
        size = mkListColumnsCount elemRep
        QTypeRepList elemRepRes = queryGetQTypeRep r
        sizeRes = mkListColumnsCount elemRepRes
    in
        [SqlStmWithSub [1..sizeRes] (replaceExpr SqlStmVar (SelectSVars [1..size]) (head $ queryToSQL $ f ASTVar)) (head $ queryToSQL lx)]
queryToSQL (ASTGetEntity ref) = [SelectTable $ entityGetBackendName ref]
queryToSQL (ASTInsertEntity x ref) = [SqlInsert (entityGetBackendName ref) (head $ queryToSQL x)]
queryToSQL (ASTInsertEntityMapAK x ref) = 
    [ SelectS [SqlLastInsertedId]
    , SqlInsertPartial (entityGetBackendName ref) [2..((mkListColumnsCount $ queryGetQTypeRep x) +1)] (head $ queryToSQL x)]
queryToSQL (ASTQValues m) = 
    let
        (QTypeRepMap keyRep valRep) = queryGetQTypeRep m
        size = mkListColumnsCount valRep
        start = mkListColumnsCount keyRep
    in
        [SelectCols [(start+1)..(start + size)] (head $ queryToSQL m)]
queryToSQL (ASTProjection i s) = [SelectCols [i] (head $ queryToSQL s)]


hqio2Sql :: HQIO (Query a) -> [Text]
hqio2Sql (HQIO x) = map sql2Text $ queryToSQL $ fst $ runState x HQIOState



mkListColumns :: QTypeRep -> [Text]
mkListColumns (QTypeRepProd l) = concatMap mkListColumns l
mkListColumns (QTypeRepNewType x) = mkListColumns x
mkListColumns QTypeRepInt = ["INT"]
mkListColumns QTypeRepText = ["TEXT"]

mkListColumnsCount :: QTypeRep -> Int
mkListColumnsCount (QTypeRepProd l) = sum $ map mkListColumnsCount l
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


sqlValue2QueryRawRes :: [[SqlValue]] -> QueryRawRes
sqlValue2QueryRawRes li =
    let
        l = concat li
        trans (SqlByteString bs) = bs
    in 
        QueryRawResSimple $ map trans l

