{-# LANGUAGE OverloadedStrings #-}

import HQueries.Queries
import HQueries.Sqlite

import qualified Data.Text as T

number = Entity WriteAccessFull (EntityRef "inteiro" :: EntityRef Integer)

numbers = Entity WriteAccessFull (EntityRef "inteiros" :: EntityRef [Integer])

main :: IO ()
main = do
    backend <- createSqliteBackend "test.db"
    migrateSchema backend [EntityObj number, EntityObj numbers]

    let dostuff x = do
        putStrLn $ T.unpack $ getBackendCode backend x
        res <- hQuery backend x
        putStrLn $ show res
  
    let f = \x -> x + toQuery (2 :: Integer)
    
    dostuff $ return $ f $ toQuery 3
    
    let q2 = getEntity number
    dostuff $ q2

    dostuff $ f `fmap` q2

    dostuff $ insertEntity 3 numbers
    dostuff $ insertEntity 4 numbers

    let q4 = getEntity numbers
    dostuff q4
    

    dostuff $ do
        v <- q4
        return $ qmap f v
    --                            [----------------]  [------------------]
    --                     SELECT ( (x) + 1      ) FROM  inteiros


