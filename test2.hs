{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell#-}

import HQueries.Queries
import HQueries.Sqlite
import HQueries.TH
import Data.Text (Text)

data Session = Session{user :: Text, secret :: Text} deriving Show

sessions :: Entity Session
sessions = Entity "sessions"

$(deriveQType ''Session)


main :: IO ()
main = do
    backend <- createSqliteBackend "test.db"

    let q = toQuery $ Session "coiso" "coiso" -- queryEntity sessions
    putStrLn $ show $ getBackendCode backend q
    res <- hQuery backend $ q
    putStrLn $ show res
