{-# LANGUAGE GADTs, RankNTypes #-}

module HQueries.Internal(
      Query(..)
    , QueryRawRes(..)
    , QBackend
    , hQuery
    , getBackendCode
    , Entity(..)
    , parseQueryRes
    , QType
    , toQuery
    , QTypeObj (..)
) where

import qualified Data.ByteString as BS
import Data.Text (Text)

data Entity a = Entity Text deriving (Show)

class QType a where
    toQuery :: a -> (Query a)
    parseQueryRes :: QueryRawRes -> (a, QueryRawRes)

data QTypeObj = forall a. (QType a) => QTypeObj a

data Query z where
    ASTProdTypeLit :: [QTypeObj] -> Query b
    ASTIntLit :: Integer -> Query Integer
    ASTTextLit :: Text -> Query Text
    ASTListLit :: forall a. QType a => [a] -> Query [a]
    ASTQMap :: (Query a -> Query b) -> Query [a] -> Query [b]
    ASTVar :: Query a
    ASTPlusInt :: Query Integer -> Query Integer -> Query Integer
    ASTGetEntity :: Entity a -> Query a

data QueryRawRes = QueryRawResSimple [BS.ByteString] deriving Show


class QBackend a where
    hQuery :: QType b => a -> Query b -> IO b
    getBackendCode :: QType b => a -> Query b -> Text


