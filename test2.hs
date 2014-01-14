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
    -- let q = toQuery $ Session "coiso" "coiso" 
    let a = getEntity sessions -- return q
    putStrLn $ show $ getBackendCode backend (a)
    res <- hQuery backend $ a
    putStrLn $ show res
