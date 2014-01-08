

import Queries

data Person = Person { firstName :: String
                        , lastName :: String
}


number :: Entity Integer
number = Entity "inteiro"

numbers :: Entity [Integer]
numbers = Entity "inteiros"

main :: IO ()
main = do
    backend <- createSqliteBackend "test.db"
    let f = \x -> x + toQuery (2 :: Integer)
    
    let q = f $ toQuery 3
    putStrLn $ getBackendCode backend q
    res <- hQuery backend $ q
    putStrLn $ show res
    
    let q2 = queryEntity number
    putStrLn $ getBackendCode backend q2
    res2 <- hQuery backend $ q2
    putStrLn $ show res2

    let q3 = f q2
    putStrLn $ getBackendCode backend q3
    res3 <- hQuery backend $ q3
    putStrLn $ show res3

    let q4 = queryEntity numbers
    putStrLn $ getBackendCode backend q4
    res4 <- hQuery backend $ q4
    putStrLn $ show res4
    

    let q5 = qmap f q4
    putStrLn $ getBackendCode backend q5
    res5 <- hQuery backend $ q5
    putStrLn $ show res5
        --putStrLn $ queryToSQL $ dbMap (+ (JustValue 1)) $ queryEntity numeros
    --                            [----------------]  [------------------]
    --                     SELECT ( (x) + 1      ) FROM  inteiros


