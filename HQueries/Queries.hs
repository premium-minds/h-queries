{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

module HQueries.Queries (
      Query
    , toQuery
    , hQuery
    , getBackendCode
    , qmap
    , getEntity
    , insertEntity
    , insertEntityMapAK
    , migrateSchema
    , Entity(..)
    , EntityObj(..)
    , EntityRef(..)
    , deriveQType

    , WriteAccessFull(..)
    , AutoKeysTypeOnly(..)
) where 

import Data.List
import Data.Text (Text)

import HQueries.Internal
import HQueries.TH


instance Num (Query Integer) where
    x + y = ASTPlusInt x y
    fromInteger i = toQuery i

qmap :: (Query a -> Query b) -> Query [a] -> Query [b]
qmap f lx = ASTQMap f lx

