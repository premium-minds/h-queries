{-# LANGUAGE TemplateHaskell #-}
module HQueries.TH(
     deriveQType
    ,deriveQKey
) where

import Language.Haskell.TH
import HQueries.Internal
import Data.Text (Text)

con2ArgTypes :: Con -> [Type] 
con2ArgTypes (NormalC _ subs) = map (\(_,x) -> x) subs 
con2ArgTypes (RecC _ subs) = map (\(_,_,x) -> x) subs 

con2Vars :: String -> Con -> Q [Name]
con2Vars z c = mapM (\_ -> newName z) (con2ArgTypes c) 


con2Name :: Con -> Name
con2Name (NormalC name _) = name
con2Name (RecC name _) = name

con2clause_toQuery :: Con -> ClauseQ
con2clause_toQuery c = do
    vars <- con2Vars "x" c 
    let n = con2Name c
    clause 
        [conP n (map varP vars)] 
        (normalB $ appE 
            (conE 'ASTProdTypeLit) 
            (listE $ map (\x -> [|QTypeObj $(varE x)|]) vars)
        )
        []
    
con2clause_parseQueryRes :: Con -> ClauseQ
con2clause_parseQueryRes c = do
    bs <- newName "bs"
    xs <- con2Vars "x" c
    rs <- con2Vars "r" c
    let n = con2Name c
    let l = zip3 xs rs (bs:(init rs))
    clause
        [varP bs]
        (normalB $ letE
            (map (\(x,r,last) -> valD (tupP [varP x, varP r]) (normalB [|parseQueryRes $(varE last)|]) []) l)
            [|($(applyArgs (conE n) (map varE xs)), $(varE (last rs)))|]
        )
        []

type2QTypeRep :: Type -> ExpQ -- QTypeRep
type2QTypeRep (ConT x) | x == ''Integer = [|QTypeRepInt|]
type2QTypeRep (ConT x) | x == ''Text = [|QTypeRepText|]

con2clause_getQTypeRep :: Con -> ClauseQ
con2clause_getQTypeRep c =
    clause
        [wildP]
        (normalB $ appE (conE 'QTypeRepProd) (listE $ map type2QTypeRep $ con2ArgTypes c ) )
        []

applyArgs :: ExpQ -> [ExpQ] -> ExpQ
applyArgs f args = foldr (\a l -> appE l a) f args

deriveQKey :: Name -> DecsQ
deriveQKey tyName = do
    let txtName = ''Text
    (TyConI (NewtypeD [] _ [] (NormalC consName [(NotStrict, (ConT txtName))]) [])) <- reify tyName
    x <- newName "x"
    iDec <- instanceD (return [])
        (appT (conT ''QKey) (conT tyName))
        [
            funD 'key2text [
                clause [conP consName [varP x]] (normalB $ varE x) []
            ],
            funD 'text2key [
                clause [varP x] (normalB $ appE (conE consName) (varE x)) []
            ]
        ]
    qtDec <- deriveQType tyName
    return $ qtDec ++ [iDec]
    

deriveQType :: Name -> DecsQ
deriveQType tyName = do
    (TyConI tpDec) <- reify tyName
    x <- newName "x"
    iDec <- case tpDec of
            (DataD _ _ [] [con] _) ->
                instanceD (return [])
                    (appT (conT ''QType) (conT tyName) )
                    [
                        funD 'toQuery [
                            con2clause_toQuery con
                        ],
                        funD 'parseQueryRes [
                            con2clause_parseQueryRes con
                        ],
                        funD 'getQTypeRep [
                            con2clause_getQTypeRep con
                        ]
                    ]
            (NewtypeD [] _ [] (NormalC conName [(NotStrict, (ConT conType))]) []) ->
                instanceD (return [])
                    (appT (conT ''QType) (conT tyName) )
                    [
                        funD 'toQuery [
                            clause [conP conName [varP x]] (normalB $ [|ASTNewTypeLit $ toQuery $(varE x)|]) []
                        ],
                        funD 'parseQueryRes [
                            clause [varP x] (normalB [|let (a,b) = parseQueryRes $(varE x) in ( $(conE conName) a, b)|]) []
                        ],
                        funD 'getQTypeRep [
                            clause [wildP] (normalB $ appE (conE 'QTypeRepNewType) (type2QTypeRep $ ConT conType) ) []
                        ]
                    ]
    return [iDec]


