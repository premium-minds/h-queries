{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell#-}

import HQueries.Queries
import HQueries.Sqlite
import HQueries.TH
import Data.Text (Text)

import HQueries.Internal

data Session = Session{user :: Text, secret :: Text} deriving Show

$(deriveQType ''Session)

sessions :: EntityRW [Session]
sessions = EntityRW "sessions"


main :: IO ()
main = do
    backend <- createSqliteBackend "test.db"
    migrateSchema backend [Entity sessions]

    let dostuff x = do
        putStrLn $ show $ getBackendCode backend x
        res <- hQuery backend x
        putStrLn $ show res

    let a = getEntity sessions
    dostuff (return $ toQuery $ Session "1" "2")
    hQuery backend $ append (toQuery $ Session "ola" "ola") sessions

    dostuff a
