{-# LANGUAGE GADTs, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module HQueries.SqlUtils(
     hqio2Sql
    ,sqlValue2QueryRawRes
    ,entity2migration
)where

import HQueries.Internal
import Database.HDBC
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.State

queryToSQL :: Query a -> [Text]
queryToSQL (ASTTextLit t) = [T.concat ["select '", T.replace "'" "''" t, "'"]]
queryToSQL (ASTProdTypeLit l) = ["select " `T.append` T.intercalate ", " (map (\(QTypeObj x) -> T.concat["(", head $ queryToSQL $ toQuery x, ")"])  l)]
queryToSQL (ASTListLit l) = [T.intercalate " union all " ((head . queryToSQL . toQuery) `map` l )]
queryToSQL (ASTIntLit i) = [T.concat ["select ", T.pack $ show i]]
queryToSQL (ASTPlusInt x y) = [T.concat ["select (", head $ queryToSQL x, ")+(", head $ queryToSQL y, ")"]]
queryToSQL ASTVar = ["x1"]
queryToSQL (ASTGetEntity ref) = [T.concat ["select * from ", entityGetBackendName ref]]
queryToSQL (ASTQMap f lx) = [T.concat ["select (", (head $ queryToSQL $ f ASTVar), ") from (", head $ queryToSQL lx, ")"]]
queryToSQL (ASTInsertEntity x ref) = [T.concat ["insert into ", entityGetBackendName ref, " ", head $ queryToSQL x]]
queryToSQL (ASTInsertEntityMapAK x ref) = ["SELECT last_insert_rowid()", T.concat[
                                                  "insert into "
                                                , entityGetBackendName ref
                                                , "("
                                                , T.intercalate "," (mkListColumnsNamesInsertMapAK $ queryGetQTypeRep x)
                                                , ") "
                                                , head $ queryToSQL x
                                                ]]

hqio2Sql :: HQIO (Query a) -> [Text]
hqio2Sql x = queryToSQL $ fst $ runState x HQIOState


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

mkAllNames = (map (\i -> T.pack $ "x" ++ show i) [1..])

mkListColumnsNamesInsertMapAK x = take (mkListColumnsCount x) (tail mkAllNames)

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

