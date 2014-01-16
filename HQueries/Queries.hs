{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

module HQueries.Queries (
     Query
    ,toQuery
    ,hQuery
    ,getBackendCode
    ,qmap
    ,EntityRW(..)
    ,getEntity
    ,append
    ,migrateSchema
    ,Entity(..)
) where 

import Data.List
import Data.Text (Text)

import HQueries.Internal


instance Num (Query Integer) where
    x + y = ASTPlusInt x y

qmap :: (Query a -> Query b) -> Query [a] -> Query [b]
qmap f lx = ASTQMap f lx

