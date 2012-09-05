{-# LANGUAGE TypeOperators #-}
module IRTS.CodegenLLVM where

import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.CodegenCommon
import Core.TT

import qualified LLVM.Wrapper.Core as L
import qualified LLVM.Wrapper.BitWriter as L

import Data.Maybe

codegenLLVM :: [(Name, LDecl)] ->
            String -> -- output file name
            Bool ->   -- generate executable if True, only .o if False 
            [FilePath] -> -- include files
            String -> -- extra compiler flags
            DbgLevel ->
            IO ()
codegenLLVM defs out exec incs libs dbg
    = do let tagged = addTags defs
         let ctxtIn = addAlist tagged emptyContext
         let checked = checkDefs ctxtIn tagged
         case checked of
           OK c -> L.withModule "" $ \m -> do
                        mapM_ (toLLVMDecl m . snd) defs
                        --mapM_ (toLLVMDef m . snd) defs
                        L.writeBitcodeToFile m out
           Error e -> fail $ "Can't happen: Something went wrong in codegenLLVM\n" ++ show e

idrValueTy :: L.Type
idrValueTy = L.structType [L.int32Type, L.pointerType L.int8Type 0] False

idrFuncTy :: Int -> L.Type
idrFuncTy n = L.functionType (L.pointerType idrValueTy 0)
              (replicate n (L.pointerType idrValueTy 0)) False

toLLVMDecl :: L.Module -> LDecl -> IO L.Value
toLLVMDecl m (LConstructor name tag _) = L.addGlobal m idrValueTy (show name)
toLLVMDecl m (LFun name args def)
    = do f <- L.addFunction m (show name) $ idrFuncTy $ length args
         ps <- L.getParams f
         mapM (uncurry L.setValueName) $ zip ps (map show args)
         return f


toLLVMDef :: L.Module -> LDecl -> IO ()
toLLVMDef m (LConstructor _ _ _) = return ()
toLLVMDef m (LFun name args exp)
    = L.withBuilder $ \b -> do
        f <- L.getNamedFunction m (show name)
        bb <- L.appendBasicBlock f "entry"
        L.positionAtEnd b bb
        params <- L.getParams f
        value <- toLLVMExp m f b params exp
        L.buildRet b value
        return ()

buildCon :: Int -> [L.Value] -> IO L.Value
buildCon tag args = undefined

-- TODO: Runtime error
-- TODO: Compiletime warning
buildCaseFailBlock :: L.Value -> IO L.BasicBlock
buildCaseFailBlock f
    = L.withBuilder $ \b -> do
        bb <- L.appendBasicBlock f "caseFail"
        L.positionAtEnd b bb
        null <- L.constPointerNull (L.pointerType idrValueTy 0)
        L.buildRet b null
        return bb

buildCaseBlock endBlock m f s e
    = L.withBuilder $ \b -> do
        bb <- L.appendBasicBlock f "case" -- TODO: Better name
        L.positionAtEnd b bb
        value <- toLLVMExp m f b s e
        L.buildBr b endBlock
        return (bb, value)

toLLVMExp :: L.Module ->  -- Current module
             L.Value ->   -- Current function
             L.Builder -> -- IR Cursor
            [L.Value] ->  -- De Bruijn levels
            LExp ->       -- Expression to process
            IO L.Value
toLLVMExp m f b s (LV var)
    = case var of
        Loc level -> return $ s !! level
        Glob name -> L.getNamedGlobal m (show name)
toLLVMExp m f b s (LApp _ name exps)
    = do callee <- L.getNamedFunction m (show name)
         args <- mapM (toLLVMExp m f b s) exps
         L.buildCall b callee args ""
toLLVMExp m f b s (LLet name value body)
    = do v <- toLLVMExp m f b s value
         toLLVMExp m f b (s ++ [v]) body
toLLVMExp m f b s (LCon tag _ exps)
    = mapM (toLLVMExp m f b s) exps >>= buildCon tag
toLLVMExp m f b s (LCase interrogatee alts)
    = undefined
    -- = do let defaults = mapMaybe (\alt -> case alt of
    --                                         LDefaultCase exp -> Just exp
    --                                         _ -> Nothing)
    --                       alts
    --      endBlock <- L.appendBasicBlock f "caseEnd"
    --      defaultBlock <- if null defaults
    --                      then (buildCaseFailBlock f)
    --                      else buildCaseBlock endBlock m f s $ defaults !! 0
    --      value <- toLLVMExp m f b s interrogatee
    --      tagVal <- L.buildStructGEP b value 0 "tag"
    --      switch <- L.buildSwitch b tagVal defaultBlock (fromIntegral $ length alts)
    --      incoming <- mapM (buildAlt switch endBlock) $ filter (\alt -> case alt of
    --                                                                      LDefaultCase _ -> False
    --                                                                      _ -> True)
    --                                                           alts
    --      phi <- L.buildPhi b idrValueTy "caseResult"
    --      L.addIncoming phi incoming
    --      return phi
    -- where
    --   buildAlt :: L.Value -> L.BasicBlock -> LAlt -> IO (L.Value, L.BasicBlock)
    --   buildAlt s e (LConstCase const exp)
    --       = do pair <- buildCaseBlock e m f s exp
               
    --   buildAlt s e (LConCase tag _ args exp) = undefined
