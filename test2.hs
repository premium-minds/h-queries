{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell#-}

import HQueries.Queries
import HQueries.Sqlite
import HQueries.TH
import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as Map

data Session = Session{user :: Text, secret :: Text} deriving Show

newtype SessionId  = SessionId Text deriving Show

$(deriveQType ''Session)
$(deriveQKey ''SessionId)

sessions = EntityMap WriteAccessFull AutoKeysTypeOnly (EntityRef "sessions" :: EntityRef (Map SessionId Session))


main :: IO ()
main = do
    backend <- createSqliteBackend "test.db"
    migrateSchema backend [EntityObj sessions]

    let dostuff x = do
        putStrLn $ T.unpack $ getBackendCode backend x
        res <- hQuery backend x
        putStrLn $ show res

    let a = getEntity sessions
    dostuff (return $ toQuery $ Session "1" "2")
    dostuff $ insertEntityMapAK (toQuery $ Session "ola" "ola") sessions
    return ()
    dostuff a
