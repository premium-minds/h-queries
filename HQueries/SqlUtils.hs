{-# LANGUAGE GADTs, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module HQueries.SqlUtils where

import HQueries.Internal
import Database.HDBC
import Data.Text (Text)
import qualified Data.Text as T

queryToSQL :: Query a -> Text
queryToSQL (ASTTextLit t) = T.concat ["select '", T.replace "'" "''" t, "'"]
queryToSQL (ASTProdTypeLit l) = "select " `T.append` T.intercalate ", " (map (\(QTypeObj x) -> T.concat["(", queryToSQL $ toQuery x, ")"])  l) 
queryToSQL (ASTListLit l) = T.intercalate " union all " ((queryToSQL . toQuery) `map` l )
queryToSQL (ASTIntLit i) = T.concat ["select ", T.pack $ show i]
queryToSQL (ASTPlusInt x y) = T.concat ["select (", queryToSQL x, ")+(", queryToSQL y, ")"]
queryToSQL ASTVar = "x1"
queryToSQL (ASTGetEntity (Entity ref)) = T.concat ["select * from ", ref] 
queryToSQL (ASTQMap f lx) = T.concat ["select (", (queryToSQL $ f ASTVar), ") from (", queryToSQL lx, ")"]


sqlValue2QueryRawRes :: [[SqlValue]] -> QueryRawRes
sqlValue2QueryRawRes li =
    let
        l = concat li
        trans (SqlByteString bs) = bs
    in 
        QueryRawResSimple $ map trans l

