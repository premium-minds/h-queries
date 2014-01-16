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

queryToSQL :: Query a -> Text
queryToSQL (ASTTextLit t) = T.concat ["select '", T.replace "'" "''" t, "'"]
queryToSQL (ASTProdTypeLit l) = "select " `T.append` T.intercalate ", " (map (\(QTypeObj x) -> T.concat["(", queryToSQL $ toQuery x, ")"])  l) 
queryToSQL (ASTListLit l) = T.intercalate " union all " ((queryToSQL . toQuery) `map` l )
queryToSQL (ASTIntLit i) = T.concat ["select ", T.pack $ show i]
queryToSQL (ASTPlusInt x y) = T.concat ["select (", queryToSQL x, ")+(", queryToSQL y, ")"]
queryToSQL ASTVar = "x1"
queryToSQL (ASTGetEntity ref) = T.concat ["select * from ", entityGetBackendName' ref] 
queryToSQL (ASTQMap f lx) = T.concat ["select (", (queryToSQL $ f ASTVar), ") from (", queryToSQL lx, ")"]
queryToSQL (ASTAppendEntity x ref) = T.concat ["insert into ", entityGetBackendName' ref, " ", queryToSQL x]

hqio2Sql :: HQIO (Query a) -> Text
hqio2Sql x = queryToSQL $ fst $ runState x HQIOState

flatQTypeRep :: QTypeRep -> QTypeRep
flatQTypeRep (QTypeRepProd l) = QTypeRepProd $ map flatQTypeRep l
flatQTypeRep (QTypeRepList x) = QTypeRepList $ flatQTypeRep x
flatQTypeRep o = o

prepareQTypeRep :: QTypeRep -> QTypeRep
prepareQTypeRep (QTypeRepList l) = l
prepareQTypeRep x = x

mkListColumns :: QTypeRep -> [Text]
mkListColumns (QTypeRepProd l) = concatMap mkListColumns l
mkListColumns QTypeRepInt = ["INT"]
mkListColumns QTypeRepText = ["TEXT"]


entity2migration :: Entity -> Text
entity2migration x =
    let
        p0 = entityGetQTypeRep x
        p1 = flatQTypeRep p0
        p2 = prepareQTypeRep p1
        pl = map (\(n, t) -> T.concat [n," ",t] ) $ (map (\i -> T.pack $ "x" ++ show i) [1..]) `zip` mkListColumns p2
    in
        T.concat[ "CREATE TABLE "
                , entityGetBackendName x
                , " ("
                , T.intercalate "," pl
                , ");"]


sqlValue2QueryRawRes :: [[SqlValue]] -> QueryRawRes
sqlValue2QueryRawRes li =
    let
        l = concat li
        trans (SqlByteString bs) = bs
    in 
        QueryRawResSimple $ map trans l

