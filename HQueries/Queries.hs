{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module HQueries.Queries (
     Query
    ,toQuery
    ,hQuery
    ,getBackendCode
    ,qmap
    ,queryEntity
    ,Entity(..)
) where 

import Data.List
import qualified Data.ByteString.Char8 as UTF8
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import HQueries.Internal


collectListRes :: (QueryRawRes -> (a, QueryRawRes)) -> QueryRawRes -> [a]
collectListRes f (QueryRawResSimple []) = []
collectListRes f l = let (t, r) = f l in [t] ++ collectListRes f r

instance QType Integer where
    toQuery i = ASTIntLit i
    parseQueryRes (QueryRawResSimple (bs:r)) = (read $ UTF8.unpack bs, QueryRawResSimple r)

instance QType a => QType [a] where
    toQuery l = ASTListLit l 
    parseQueryRes qr = (collectListRes parseQueryRes qr, QueryRawResSimple [])

instance QType Text where
    toQuery t = ASTTextLit t
    parseQueryRes (QueryRawResSimple (bs:r)) = (TE.decodeUtf8 bs, QueryRawResSimple r)

instance Num (Query Integer) where
    x + y = ASTPlusInt x y

queryEntity :: Entity a -> Query a
queryEntity e = ASTGetEntity e

qmap :: (Query a -> Query b) -> Query [a] -> Query [b]
qmap f lx = ASTQMap f lx

