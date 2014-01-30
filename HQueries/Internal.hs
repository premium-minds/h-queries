{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, FlexibleContexts #-}

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
    , HQIO(..)
    , HQIOState(..)
    , getEntity
    , insertEntity
    , insertEntityMapAK
    , entityGetQTypeRep
    , queryGetQTypeRep
    , migrateSchema
    , Entity(..)
    , EntityObj(..)
    , EntityRef(..)
    , entityGetBackendName
    , text2key
    , key2text
    , QKey

    , WriteAccessFull(..)
    , AutoKeysTypeOnly(..)
) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import Control.Monad.State
import qualified Data.ByteString.Char8 as UTF8
import qualified Data.Text.Encoding as TE

import Data.Map (Map)
import qualified Data.Map as M


data HQIOState = HQIOState

newtype HQIO a = HQIO (State HQIOState a)


instance Monad HQIO where
    return x = HQIO $ return x
    (HQIO x) >>= f = HQIO $ x >>= (\x -> let HQIO y = f x in y)

instance Functor HQIO where
    fmap f (HQIO x) = HQIO $ fmap f x 

data QTypeRep =   QTypeRepInt
                | QTypeRepUnit
                | QTypeRepText
                | QTypeRepList QTypeRep
                | QTypeRepMap QTypeRep QTypeRep
                | QTypeRepProd [QTypeRep] 
                | QTypeRepNewType QTypeRep
    deriving Show


class EntityWriteAccessType a
class EntityAppendAccess a
data WriteAccessFull = WriteAccessFull
instance EntityWriteAccessType WriteAccessFull
instance EntityAppendAccess WriteAccessFull

class EntityAutoKeysType a
class EntityAutoKeys a
data AutoKeysTypeNone = AutoKeysTypeNone
instance EntityAutoKeysType AutoKeysTypeNone
data AutoKeysTypeOnly = AutoKeysTypeOnly
instance EntityAutoKeysType AutoKeysTypeOnly
instance EntityAutoKeys AutoKeysTypeOnly
data AutoKeysTypeBoth = AutoKeysTypeBoth
instance EntityAutoKeysType AutoKeysTypeBoth
instance EntityAutoKeys AutoKeysTypeBoth

data EntityRef a where EntityRef :: QType a => Text -> EntityRef a

data EntityObj where EntityObj :: Entity a b c -> EntityObj

data Entity a b c where
    Entity :: (QType c, EntityWriteAccessType a) => a -> EntityRef c -> Entity a AutoKeysTypeNone c
    EntityMap :: (QType (Map x y), EntityWriteAccessType a, EntityAutoKeysType b) => a -> b -> EntityRef (Map x y) -> Entity a b (Map x y)

getEntity :: QType c => Entity a b c -> HQIO (Query c)
getEntity e = return $ ASTGetEntity e

insertEntity :: (QType c, EntityAppendAccess a) => Query c -> Entity a b [c] -> HQIO (Query ())
insertEntity x e = return $ ASTInsertEntity x e

insertEntityMapAK :: (QKey k, QType c, EntityAppendAccess a, EntityAutoKeys b) => Query c -> Entity a b (Map k c) -> HQIO (Query k)
insertEntityMapAK x e = return $ ASTInsertEntityMapAK x e



entityGetQTypeRep :: forall a b c. Entity a b c -> QTypeRep
entityGetQTypeRep (Entity _ _) = getQTypeRep (undefined :: c)
entityGetQTypeRep (EntityMap _ _ _) = getQTypeRep (undefined :: c)

queryGetQTypeRep :: forall c. (QType c) => Query c -> QTypeRep
queryGetQTypeRep _ = getQTypeRep (undefined :: c)

entityGetBackendName :: Entity a b c -> Text
entityGetBackendName (Entity _ (EntityRef x)) = x
entityGetBackendName (EntityMap _ _ (EntityRef x)) = x

class QType a where
    toQuery :: a -> (Query a)
    parseQueryRes :: QueryRawRes -> (a, QueryRawRes)
    getQTypeRep :: a -> QTypeRep

class (QType a, Ord a) => QKey a where
    key2text :: a -> Text
    text2key :: Text -> a

data QTypeObj = forall a. (QType a) => QTypeObj a

data Query z where
    ASTUnit :: Query ()
    ASTProdTypeLit :: [QTypeObj] -> Query b
    ASTNewTypeLit :: Query a -> Query b
    ASTIntLit :: Integer -> Query Integer
    ASTTextLit :: Text -> Query Text
    ASTListLit :: QType a => [a] -> Query [a]
    ASTMapLit :: (QType k ,QType a) => Map k a -> Query (Map k a)
    ASTQMap :: (QType a, QType b) => (Query a -> Query b) -> Query [a] -> Query [b]
    ASTVar :: Query a
    ASTPlusInt :: Query Integer -> Query Integer -> Query Integer
    ASTGetEntity :: (QType c) => Entity a b c -> Query c
    ASTInsertEntity :: (QType c, EntityAppendAccess a) => Query c -> Entity a b [c] -> Query ()
    ASTInsertEntityMapAK :: (QType c, QKey k, EntityAppendAccess a, EntityAutoKeys b) => Query c -> Entity a b (Map k c) -> Query k
    ASTQValues :: (QKey k, QType c) => Query (Map k c) -> Query [c]
    ASTProjection :: Int -> Query a -> Query b


data QueryRawRes = QueryRawResSimple [BS.ByteString] deriving Show

class QBackend a where
    hQuery :: QType b => a -> HQIO (Query b) -> IO b
    getBackendCode :: QType b => a -> HQIO (Query b) -> Text
    migrateSchema ::  a -> [EntityObj] -> IO ()

instance QType Integer where
    toQuery i = ASTIntLit i
    parseQueryRes (QueryRawResSimple (bs:r)) = (read $ UTF8.unpack bs, QueryRawResSimple r)
    getQTypeRep _ = QTypeRepInt


instance QType Text where
    toQuery t = ASTTextLit t
    parseQueryRes (QueryRawResSimple (bs:r)) = (TE.decodeUtf8 bs, QueryRawResSimple r)
    parseQueryRes (QueryRawResSimple rest) = error $ "can't get Text from " ++ show rest
    getQTypeRep _ = QTypeRepText

instance QType () where
    toQuery _ = ASTUnit
    parseQueryRes qr = ((), qr)
    getQTypeRep _ = QTypeRepUnit




