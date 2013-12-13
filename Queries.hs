{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module Queries (
    toAST
    , dbMap
    , queryToSQL
    , queryEntity
    , ASTNode(..)
    , DBWrapper(..)
    , Entity(..)
) where 
    import Data.List

    
    data ASTNode = ASTIntLit Integer
             | Lambda ASTNode
             | ASTVar
             | ASTPlus ASTNode ASTNode
             | GetEntity String
             | DbMap ASTNode ASTNode
        deriving Show

    data DBWrapper a = JustValue a | Var | Code ASTNode


    class DBType a where
        toAST:: a -> ASTNode
        makeVar :: (a -> b) -> a

    class DBLit a where
        toLit :: a -> ASTNode 



    instance DBLit Integer where
        toLit x = ASTIntLit x

    instance DBLit a  => DBType (DBWrapper a) where
        toAST (JustValue x) = toLit x
        toAST (Var) = ASTVar
        toAST (Code x) = x
        makeVar f = Var

    instance (DBType a, DBType b) => DBType (a->b) where
        toAST f =
            let 
                x = makeVar f 
            in
                Lambda (toAST $ f x)


    instance (DBType (DBWrapper a)) => Num (DBWrapper a) where
        (+) x y = Code $ ASTPlus (toAST x) (toAST y)



    data Entity a = Entity String
    data Query a = Query ASTNode

    queryEntity :: Entity a -> Query a
    queryEntity (Entity refName) = Query (GetEntity refName)

    dbMap :: (DBType a, DBType b) => (a->b) -> (Query a) -> (Query b)
    dbMap f  (Query node) = Query (DbMap (toAST f) node)

    queryToSQL :: Query a -> String
    queryToSQL (Query node) = queryToSQL' node
    
    queryToSQL' :: (ASTNode) -> String
    queryToSQL' (GetEntity refName) = "select * from " ++ refName
    queryToSQL' (ASTIntLit x) = show x
    queryToSQL' (ASTPlus x y) = queryToSQL' x ++ " + " ++ queryToSQL' y
    queryToSQL' (ASTVar)      = "x"
    queryToSQL' (DbMap (Lambda lNode) jNode) = "select " ++ queryToSQL' lNode ++ " from (" ++ queryToSQL' jNode ++ ") as tretas"
    queryToSQL' x = error $ "Cannot compile " ++ show x ++ "to sql"


        
    








