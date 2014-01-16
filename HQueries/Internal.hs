{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

module HQueries.Internal(
      Query(..)
    , QueryRawRes(..)
    , QBackend
    , hQuery
    , getBackendCode
    , parseQueryRes
    , QType
    , toQuery
    , getQTypeRep
    , QTypeRep(..)
    , QTypeObj (..)
    , HQIO
    , HQIOState(..)
    , EntityRW(..)
    , getEntity
    , append
    , entityGetQTypeRep
    , migrateSchema
    , Entity(..)
    , entityGetBackendName
    , entityGetBackendName'
) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import Control.Monad.State
import qualified Data.ByteString.Char8 as UTF8
import qualified Data.Text.Encoding as TE

data HQIOState = HQIOState

type HQIO = State HQIOState

data QTypeRep =   QTypeRepInt
                | QTypeRepUnit
                | QTypeRepText
                | QTypeRepList QTypeRep
                | QTypeRepProd [QTypeRep] 
    deriving Show

data Entity = forall a. EntityClass a => Entity a

data EntityRW a = EntityRW Text

class EntityRead a where
    getEntity :: QType b => a b -> HQIO (Query b)

class EntityAppend a where
    append :: QType b => Query b -> a [b] -> HQIO (Query ())

class EntityClass a where
    entityGetQTypeRep' :: a -> QTypeRep
    entityGetBackendName' :: a -> Text

instance EntityRead EntityRW where
    getEntity e = return $ ASTGetEntity e

instance EntityAppend EntityRW where
    append x e = return $ ASTAppendEntity x e

instance QType a => EntityClass (EntityRW a) where
    entityGetQTypeRep' _ = getQTypeRep (undefined :: a) 
    entityGetBackendName' (EntityRW n) = n


entityGetQTypeRep (Entity x) = entityGetQTypeRep' x
entityGetBackendName (Entity x) = entityGetBackendName' x

class QType a where
    toQuery :: a -> (Query a)
    parseQueryRes :: QueryRawRes -> (a, QueryRawRes)
    getQTypeRep :: a -> QTypeRep

data QTypeObj = forall a. (QType a) => QTypeObj a

data Query z where
    ASTUnit :: Query ()
    ASTProdTypeLit :: [QTypeObj] -> Query b
    ASTIntLit :: Integer -> Query Integer
    ASTTextLit :: Text -> Query Text
    ASTListLit :: forall a. QType a => [a] -> Query [a]
    ASTQMap :: (Query a -> Query b) -> Query [a] -> Query [b]
    ASTVar :: Query a
    ASTPlusInt :: Query Integer -> Query Integer -> Query Integer
    ASTGetEntity :: forall a b. (EntityRead a, EntityClass (a b)) => a b -> Query b
    ASTAppendEntity :: forall a b. (EntityAppend a, EntityClass (a [b])) => Query b -> a [b] -> Query ()


data QueryRawRes = QueryRawResSimple [BS.ByteString] deriving Show

class QBackend a where
    hQuery :: QType b => a -> HQIO (Query b) -> IO b
    getBackendCode :: QType b => a -> HQIO (Query b) -> Text
    migrateSchema ::  a -> [Entity] -> IO ()

instance QType Integer where
    toQuery i = ASTIntLit i
    parseQueryRes (QueryRawResSimple (bs:r)) = (read $ UTF8.unpack bs, QueryRawResSimple r)
    getQTypeRep _ = QTypeRepInt

instance QType a => QType [a] where
    toQuery l = ASTListLit l 
    parseQueryRes qr = (collectListRes parseQueryRes qr, QueryRawResSimple [])
    getQTypeRep _ = QTypeRepList (getQTypeRep (undefined :: a))

instance QType Text where
    toQuery t = ASTTextLit t
    parseQueryRes (QueryRawResSimple (bs:r)) = (TE.decodeUtf8 bs, QueryRawResSimple r)
    getQTypeRep _ = QTypeRepText

instance QType () where
    toQuery _ = ASTUnit
    parseQueryRes qr = ((), qr)
    getQTypeRep _ = QTypeRepUnit

collectListRes :: (QueryRawRes -> (a, QueryRawRes)) -> QueryRawRes -> [a]
collectListRes f (QueryRawResSimple []) = []
collectListRes f l = let (t, r) = f l in [t] ++ collectListRes f r


