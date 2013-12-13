

import Queries

data Person = Person { firstName :: String
                        , lastName :: String
}


numeros :: Entity (DBWrapper Integer)
numeros = Entity "inteiros"

main :: IO ()
main = do 
    putStrLn $ queryToSQL $ queryEntity numeros
    putStrLn $ queryToSQL $ dbMap (+ (JustValue 1)) $ queryEntity numeros
    --                            [----------------]  [------------------]
    --                     SELECT ( (x) + 1      ) FROM  inteiros


