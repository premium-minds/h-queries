{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

module HQueries.Queries (
      Query
    , toQuery
    , hQuery
    , getBackendCode
    , qmap
    , qvalues
    , qmapValues
    , qMapToList
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
    , QType
    , QTypeRep(..)
    , QTypeRepProdHead(..)
    , getQTypeRep

    , WriteAccessFull(..)
    , AutoKeysTypeOnly(..)
) where 

import Data.List
import Data.Text (Text)

import HQueries.Internal
import HQueries.TH

import Control.Lens
import Control.Lens.Tuple
import Control.Applicative
import Control.Lens.Combinators
import Control.Lens.Indexed
import Control.Lens.Type

import Data.Map (Map)
import qualified Data.Map as M

instance Num (Query Integer) where
    x + y = ASTPlusInt x y
    fromInteger i = toQuery i

instance QType a => QType [a] where
    toQuery l = ASTListLit (map toQuery l) 
    parseQueryRes qr = (collectListRes parseQueryRes qr, QueryRawResSimple [])
    getQTypeRep _ = QTypeRepList (getQTypeRep (undefined :: a))

instance (QKey k, QType a) => QType (Map k a) where
    toQuery mp = ASTMapLit $ map (\(x, y) -> (toQuery x, toQuery y)) $ M.toList mp 
    parseQueryRes qr = (M.fromList $ collectListRes parseQueryRes qr, QueryRawResSimple [])
    getQTypeRep _ = QTypeRepMap (getQTypeRep (undefined :: k)) (getQTypeRep (undefined :: a))

collectListRes :: (QueryRawRes -> (a, QueryRawRes)) -> QueryRawRes -> [a]
collectListRes f (QueryRawResSimple []) = []
collectListRes f l = let (t, r) = f l in [t] ++ collectListRes f r


instance (QType a, QType b) => QType (a,b) where
    toQuery (x, y) = ASTProdTypeLit [QueryObj $ toQuery x, QueryObj $ toQuery y]
    parseQueryRes qr = 
        let
            (x, r0) = parseQueryRes qr
            (y, r) = parseQueryRes r0
        in
            ((x,y),r)
    getQTypeRep _ = QTypeRepProd QTypeRepProdHeadTuple Nothing [getQTypeRep (undefined :: a), getQTypeRep (undefined :: b)]

qTuple2 :: Query a -> Query b -> Query (a, b)
qTuple2 q1 q2 = ASTProdTypeLit [QueryObj q1, QueryObj q2]

instance (QType a, QType b, QType a') => Field1 (Query (a, b)) (Query (a', b)) (Query a) (Query a') where
    _1 k q = indexed k (0 :: Int) (ASTProjection 1 q) <&> \x' -> qTuple2 x' (ASTProjection 2 q) -- \a' -> (a',b)

instance (QType a, QType b, QType b') => Field2 (Query (a, b)) (Query (a, b')) (Query b) (Query b') where
    _2 k q = indexed k (1 :: Int) (ASTProjection 2 q) <&> \y' -> qTuple2 (ASTProjection 1 q) y' -- \b' -> (a,b')

instance QType a => QType (Maybe a) where
    toQuery Nothing = ASTNothing
    toQuery (Just x) = ASTJust (toQuery x)
    getQTypeRep _ = QTypeRepMaybe (getQTypeRep (undefined :: a))

qmap ::(QType a, QType b) => (Query a -> Query b) -> Query [a] -> Query [b]
qmap f lx = ASTQMap f lx

qvalues ::(QKey a, QType b) => Query (Map a b) -> Query [b]
qvalues m = qmap (view _2) (qMapToList m)

qMapToList :: (QKey k, QType a) => Query (Map k a) -> Query [(k, a)]
qMapToList m = ASTQMapToList m

qListToMap :: (QKey k, QType a) => Query [(k, a)] -> Query (Map k a)
qListToMap l = ASTQListToMap l

qmapValues :: (QKey k, QType a, QType b) => (Query a -> Query b) -> Query (Map k a) -> Query (Map k b)
qmapValues f m = qListToMap $ qmap (\x -> over _2 f x) $ qMapToList m

