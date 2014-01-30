{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

module HQueries.Queries (
      Query
    , toQuery
    , hQuery
    , getBackendCode
    , qmap
    , qvalues
    , getEntity
    , insertEntity
    , insertEntityMapAK
    , migrateSchema
    , Entity(..)
    , EntityObj(..)
    , EntityRef(..)
    , deriveQType
    , deriveQKey
    , makeQLenses
    , HQIO
    , QBackend

    , WriteAccessFull(..)
    , AutoKeysTypeOnly(..)
) where 

import Data.List
import Data.Text (Text)

import HQueries.Internal
import HQueries.TH

import Data.Map (Map)
import qualified Data.Map as M

instance Num (Query Integer) where
    x + y = ASTPlusInt x y
    fromInteger i = toQuery i

instance QType a => QType [a] where
    toQuery l = ASTListLit l 
    parseQueryRes qr = (collectListRes parseQueryRes qr, QueryRawResSimple [])
    getQTypeRep _ = QTypeRepList (getQTypeRep (undefined :: a))

instance (QKey k, QType a) => QType (Map k a) where
    toQuery l = ASTMapLit l 
    parseQueryRes qr = (M.fromList $ collectListRes parseQueryRes qr, QueryRawResSimple [])
    getQTypeRep _ = QTypeRepMap (getQTypeRep (undefined :: k)) (getQTypeRep (undefined :: a))

collectListRes :: (QueryRawRes -> (a, QueryRawRes)) -> QueryRawRes -> [a]
collectListRes f (QueryRawResSimple []) = []
collectListRes f l = let (t, r) = f l in [t] ++ collectListRes f r


instance (QType a, QType b) => QType (a,b) where
    toQuery (x, y) = ASTProdTypeLit [QTypeObj x, QTypeObj y]
    parseQueryRes qr = 
        let
            (x, r0) = parseQueryRes qr
            (y, r) = parseQueryRes r0
        in
            ((x,y),r)


qmap ::(QType a, QType b) => (Query a -> Query b) -> Query [a] -> Query [b]
qmap f lx = ASTQMap f lx

qvalues ::(QKey a, QType b) => Query (Map a b) -> Query [b]
qvalues m = ASTQValues m

