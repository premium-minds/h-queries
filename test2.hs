{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell#-}

import HQueries.Queries
import HQueries.Sqlite
import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens

data Session = SessionC{_sUser :: Text, _sSecret :: Text} deriving Show

newtype SessionId  = SessionId Text deriving (Show, Eq, Ord)

$(deriveQType ''Session)
$(deriveQKey ''SessionId)

$(makeQLenses ''Session)

sessions = EntityMap WriteAccessFull AutoKeysTypeOnly (EntityRef "sessions" :: EntityRef (Map SessionId Session))


main :: IO ()
main = do
    backend <- createSqliteBackend "test.db"
    migrateSchema backend [EntityObj sessions]

    let dostuff x = do
        putStrLn $ T.unpack $ getBackendCode backend x
        res <- hQuery backend x
        putStrLn $ show res
        putStrLn "#####"

    let a = getEntity sessions
    dostuff (return $ toQuery $ SessionC "1" "2")
    dostuff $ insertEntityMapAK (toQuery $ SessionC "ola" "ola") sessions
    dostuff a

    dostuff $ return $ (view sUserQ) (toQuery $ SessionC "ola" "hello")

    dostuff $ do
        s <- getEntity sessions
        return $ qmap (view sUserQ) (qvalues s)
