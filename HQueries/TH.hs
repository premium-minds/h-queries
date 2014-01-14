{-# LANGUAGE TemplateHaskell #-}
module HQueries.TH(
    deriveQType
) where

import Language.Haskell.TH
import HQueries.Internal


con2Vars :: String -> Con -> Q [Name]
con2Vars z (NormalC _ subs) = mapM (\_ -> newName z) subs 
con2Vars z (RecC _ subs) = mapM (\_ -> newName z) subs 


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


applyArgs :: ExpQ -> [ExpQ] -> ExpQ
applyArgs f args = foldr (\a l -> appE l a) f args


deriveQType :: Name -> DecsQ
deriveQType tyName = do
    (TyConI (DataD _ _ [] [con] _)) <- reify tyName
    iDec <- instanceD (return [])
                (appT (conT ''QType) (conT tyName) )
                [
                    funD 'toQuery [
                        con2clause_toQuery con
                    ],
                    funD 'parseQueryRes [
                        con2clause_parseQueryRes con
                    ]
                ]
    return [iDec]



{-
instance QType Session where
    toQuery Session{..} = ASTProdTypeLit [QTypeObj user, QTypeObj secret]
    parseQueryRes bs = 
        let
            (user, r1) = parseQueryRes bs
            (secret, r) = parseQueryRes r1
        in
            (Session user secret, r)
-}
