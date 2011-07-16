--------------------------------------------------------------------------------
-- Preprocessor for the joinads 'docase' notation.
-- For more information see: http://www.cl.cam.ac.uk/~tp322/papers/docase.html 
-- 
-- The implementation is based on 'arrowp' preprocessor created by Ross
-- Paterson (available at http://hackage.haskell.org/package/arrowp)
-- 
-- This file defines the AST extension for joinads and implements a
-- translation from 'docase' syntax to monadic combinator (as described
-- in the paper).
-----------------------------------------------------------------------------

module JoinadsSyn (
  HsJoinadClause(..),
  translateDoCase
) where

import Language.Haskell.Syntax

data HsJoinadClause
  = JoinadClause SrcLoc [Maybe HsPat] HsGuardedAlts [HsDecl]

maliasOp, mzeroOp, mreturnOp, idOp :: HsExp
idOp = HsVar (UnQual (HsIdent "id"))
mreturnOp = HsVar (UnQual (HsIdent "return"))
mzeroOp = HsVar (UnQual (HsIdent "mzero"))
maliasOp = HsVar (UnQual (HsIdent "malias"))

morelseOp, mzipOp, mbindOp :: HsQOp
mbindOp = HsQVarOp (UnQual (HsSymbol ">>="))
mzipOp = HsQVarOp (UnQual (HsIdent "mzip"))
morelseOp = HsQVarOp (UnQual (HsIdent "morelse"))

translateDoCase :: SrcLoc -> [HsExp] -> [HsJoinadClause] -> HsExp
translateDoCase loc inpArgs clauses = 

  -- To translate 'docase' syntax, we first define new variables for argments 
  -- then translate clauses (giving them the input variables as arguments)
  -- and then construct expression that uses 'malias' on all inputs 
  -- and contains the combination of translated clauses.
  case (zip [ 1 .. ] clauses) of
    clause:clauses -> 

      -- Generate new variables and translate all clauses independently
      let argNames = [ "avar" ++ (show idx) | idx <- [ 1 .. ] ] in
      let argVars = [ HsVar $ UnQual $ HsSymbol $ name | name <- argNames ] in
      let allclauses = foldl (combineClauses argVars) (translateClause argVars clause) clauses in

      -- Add '>>= id' to the end of the body
      let body = bindExprs (HsParen allclauses) idOp in

      -- Add 'malias' calls for all input arguments and use bind to 
      -- get the referenced computation
      foldr (\(inpArg, argName) expr -> 
        (HsParen $ HsApp maliasOp (HsParen inpArg)) `bindExprs` 
          (HsParen $ HsLambda loc [HsPVar $ HsIdent argName] expr)) 
            body (zip inpArgs argNames)

    [] -> undefined

  where
    -- Translate the 'clause' and combine it using 'morelse' with 
    -- (already translated) clause 'expr'
    combineClauses args expr clause =
      HsInfixApp expr morelseOp (translateClause args clause) 
      
    translateClause args (_, JoinadClause _ _ (HsGuardedAlts _) _) = 
      -- Guards are not supported
      undefined 

    -- To translate clause, we find all inputs that are used in the pattern,
    -- combine all inputs using 'mzip', combine all patterns into tuple pattern
    -- and then add '>>= \(...) -> ...' with translated body.
    translateClause args (idx, JoinadClause loc pats (HsUnGuardedAlt body) wheres) = 
      let inputs = [ (p, a) | (Just p, a) <- zip pats args ] in
      let tmpVar = "jvar" ++ (show idx) in
      let tmpVarExp = HsVar $ UnQual $ HsSymbol $ tmpVar in
      case inputs of
        inp:inps ->
          let mergedE = foldl mzipExprs (snd inp) (map snd inps) in
          let mergedP = foldl tuplePat (fst inp) (map fst inps) in

          let matchCase = HsApp mreturnOp (HsParen body) in
          let failCase = mzeroOp in

          let cases = [ HsAlt loc mergedP (HsUnGuardedAlt matchCase) wheres,
                        HsAlt loc HsPWildCard (HsUnGuardedAlt failCase) [] ] in
          let body = HsCase tmpVarExp cases in
          HsParen (bindExprs mergedE (HsLambda loc [HsPVar $ HsIdent tmpVar] body))

    
        [] -> undefined
    
    mzipExprs e1 e2 = HsInfixApp e1 mzipOp e2
    bindExprs e cont = HsInfixApp e mbindOp cont
    tuplePat p1 p2 = HsPTuple [p1, p2]
