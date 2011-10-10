module Idris.Delaborate where

-- Convert core TT back into high level syntax, primarily for display
-- purposes.

import Idris.AbsSyntax
import Core.TT

import Debug.Trace

delab :: IState -> Term -> PTerm
delab ist tm = de [] tm
  where
    de env (App f a) = deFn env f [a]
    de env (V i)     = PRef (env!!i)
    de env (P _ n _) = PRef n
    de env (Bind n (Lam ty) sc) = PLam n (de env ty) (de (n:env) sc)
    de env (Bind n (Pi ty) sc)  = PPi Exp n (de env ty) (de (n:env) sc)
    de env (Bind n (Let ty val) sc) 
        = PLet n (de env ty) (de env val) (de (n:env) sc)
    de env (Bind n _ sc) = de (n:env) sc
    de env (Set i) = PSet 

    deFn env (App f a) args = deFn env f (a:args)
    deFn env (P _ n _) args = mkPApp n (map (de env) args)
    deFn env f args = PApp (de env f) [] (map (de env) args)

    mkPApp n args 
        | Just (ns, i) <- lookupCtxt n (idris_implicits ist)
            = PApp (PRef n) (zip ns args) (drop (length ns) args)
        | otherwise = PApp (PRef n) [] args
