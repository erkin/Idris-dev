{-# LANGUAGE TypeOperators #-}
module IRTS.CodegenLLVM (codegen) where

import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.CodegenCommon
import Core.TT

import qualified LLVM.Wrapper.Core as L
import qualified LLVM.Wrapper.BitWriter as L

import Foreign.Ptr
import Control.Monad
import Debug.Trace

codegen :: Codegen
codegen defs out exec incs libs dbg
    = L.withModule "" $ \m -> do
        declarePrimitives m
        mapM_ (toLLVMDecl m . snd) defs
        mapM_ (toLLVMDef m . snd) defs
        L.writeBitcodeToFile m out

conTypeID = 0
intTypeID = 1
floatTypeID = 2
stringTypeID = 3
unitTypeID = 4
ptrTypeID = 5

declarePrimitives :: L.Module -> IO ()
declarePrimitives m
    = do L.addFunction m "malloc" $ L.functionType (L.pointerType L.int8Type 0)
                                    [L.int64Type] False
         conTy <- L.structCreateNamed "constructor"
         valTy <- L.structCreateNamed "value"
         -- TODO: Is arity actually needed here?
         --                     Tag          Arity        Args
         L.structSetBody conTy [L.int32Type, L.int32Type, L.arrayType (L.pointerType valTy 0) 0] False
         --                     Type         Value
         L.structSetBody valTy [L.int32Type, conTy] False
         return () -- TODO: GC

idrConTy :: L.Module -> IO L.Type
idrConTy m = L.getTypeByName m "constructor"

idrValueTy :: L.Module -> IO L.Type
idrValueTy m = L.getTypeByName m "value"

idrFuncTy :: L.Module -> Int -> IO L.Type
idrFuncTy m n = idrValueTy m >>= \vt ->
                return $ L.functionType (L.pointerType vt 0) (replicate n $ L.pointerType vt 0) False

toLLVMDecl :: L.Module -> SDecl -> IO ()
toLLVMDecl m (SFun name args _ _)
    = do ty <- idrFuncTy m $ length args
         f <- L.addFunction m (show name) ty
         ps <- L.getParams f
         mapM (uncurry L.setValueName) $ zip ps (map show args)
         return ()


toLLVMDef :: L.Module -> SDecl -> IO ()
toLLVMDef m (SFun name args _ exp)
    = L.withBuilder $ \b -> do
        trace ("Compiling function " ++ (show name)) $ return ()
        f <- L.getNamedFunction m (show name)
        bb <- L.appendBasicBlock f "entry"
        L.positionAtEnd b bb
        params <- L.getParams f
        value <- toLLVMExp' m f b params exp
        L.buildRet b value
        return ()

buildAlloc :: L.Module -> L.Builder -> L.Value -> IO L.Value
buildAlloc m b bytes
    = do malloc <- L.getNamedFunction m "malloc" -- TODO: GC
         L.buildCall b malloc [bytes] ""

buildVal :: L.Module -> L.Builder -> Int -> IO L.Value
buildVal m b argCount
    = do valSize <- idrValueTy m >>= L.sizeOf
         ptrSize <- L.sizeOf (L.pointerType L.int8Type 0)
         mem <- buildAlloc m b (L.constAdd valSize
                                     (L.constMul ptrSize
                                           (L.constInt L.int64Type
                                                 (fromIntegral $ argCount) True)))
         destTy <- idrValueTy m >>= \t -> return $ L.pointerType t 0
         L.buildPointerCast b mem destTy ""

buildCon :: L.Module -> L.Builder -> Int -> [L.Value] -> IO L.Value
buildCon m b tag args
    = do val <- buildVal m b $ length args
         valTyPtr <- L.buildStructGEP b val 0 "typePtr"
         L.buildStore b (L.constInt L.int32Type conTypeID True) valTyPtr
         conPtr <- L.buildStructGEP b val 1 "constructorPtr"
         tagPtr <- L.buildStructGEP b conPtr 0 "tagPtr"
         L.buildStore b (L.constInt L.int32Type (fromIntegral tag) True) tagPtr
         arityPtr <- L.buildStructGEP b conPtr 1 "arityPtr"
         L.buildStore b (L.constInt L.int32Type (fromIntegral $ length args) True) arityPtr
         mapM (\(arg, idx) -> do
                 place <- L.buildInBoundsGEP b conPtr
                          [ L.constInt L.int32Type 0 True
                          , L.constInt L.int32Type 2 True
                          , L.constInt L.int32Type (fromIntegral idx) True
                          ] $ "arg" ++ (show idx)
                 L.buildStore b arg place) $ zip args [0..]
         return val

buildInt :: L.Module -> L.Builder -> L.Value -> IO L.Value
buildInt m b value
    = do val <- buildVal m b 0
         con <- L.buildStructGEP b val 1 ""
         intPtr <- L.buildPointerCast b con (L.pointerType L.int32Type 0) ""
         L.buildStore b value intPtr
         return val

buildFloat :: L.Module -> L.Builder -> L.Value -> IO L.Value
buildFloat m b value
    = do val <- buildVal m b 0
         con <- L.buildStructGEP b val 1 ""
         floatPtr <- L.buildPointerCast b con (L.pointerType L.doubleType 0) ""
         L.buildStore b value floatPtr
         return val

buildStr :: L.Module -> L.Builder -> L.Value -> IO L.Value
buildStr m b value
    = do val <- buildVal m b 0
         con <- L.buildStructGEP b val 1 ""
         strPtr <- L.buildPointerCast b con (L.pointerType L.int8Type 0) ""
         L.buildStore b value strPtr
         return val

-- TODO: Runtime error
buildCaseFail :: L.Module -> L.Value -> IO (L.BasicBlock, L.Value)
buildCaseFail m f
    = do bb <- L.appendBasicBlock f "caseFail"
         vt <- idrValueTy m
         return (bb, L.getUndef (L.pointerType vt 0))

buildAlt :: L.Module -> L.Value -> [L.Value] -> L.Value -> SAlt -> IO (L.BasicBlock, L.BasicBlock, L.Value)
buildAlt m f s _ (SDefaultCase body)
    = L.withBuilder $ \b -> do
        entry <- L.appendBasicBlock f "default"
        L.positionAtEnd b entry
        result <- toLLVMExp' m f b s body
        exit <- L.getInsertBlock b
        return (entry, exit, result)
buildAlt m f s ctorPtr (SConCase _ _ _ argNames body)
    = L.withBuilder $ \b -> do
        entry <- L.appendBasicBlock f "alt"
        L.positionAtEnd b entry
        args <- mapM (\(name, idx) -> do
                        argPtr <- L.buildInBoundsGEP b ctorPtr
                                  [ L.constInt L.int32Type 0 True
                                  , L.constInt L.int32Type 2 True
                                  , L.constInt L.int32Type idx True
                                  ] ""
                        L.buildLoad b argPtr $ show name)
                     $ zip argNames [0..]
        result <- toLLVMExp' m f b (s ++ args) body
        exit <- L.getInsertBlock b
        return (entry, exit, result)
buildAlt m f s _ (SConstCase _ body)
    = L.withBuilder $ \b -> do
        entry <- L.appendBasicBlock f "alt"
        L.positionAtEnd b entry
        result <- toLLVMExp' m f b s body
        exit <- L.getInsertBlock b
        return (entry, exit, result)

foreignToC :: FType -> L.Type
foreignToC ty = case ty of
                  FInt -> L.int32Type
                  FString -> L.pointerType L.int8Type 0
                  FUnit -> L.voidType
                  FPtr -> L.pointerType L.int8Type 0
                  FDouble -> L.doubleType

idrToNative :: L.Builder -> FType -> L.Value -> IO L.Value
idrToNative b ty v = do ctorPtr <- L.buildStructGEP b v 1 ""
                        primPtr <- L.buildPointerCast b ctorPtr
                                   (L.pointerType (foreignToC ty) 0) ""
                        L.buildLoad b primPtr ""

cToIdr :: L.Module -> L.Builder -> FType -> L.Value -> IO L.Value
cToIdr m b ty v = case ty of
                    FInt -> buildInt m b v
                    FString -> buildStr m b v
                    FDouble -> buildFloat m b v

ensureBound :: L.Module -> String -> FType -> [FType] -> IO L.Value
ensureBound m name rty argtys
    = do old <- L.getNamedFunction m name
         case old == nullPtr of
           True  -> L.addFunction m name $ L.functionType (foreignToC rty) (map foreignToC argtys) False
           False -> return old

lookupVar :: L.Module -> [L.Value] -> LVar -> IO L.Value
lookupVar m s (Loc level) = return $ s !! level
lookupVar m s (Glob name)
    = do val <- L.getNamedGlobal m (show name)
         when (val == nullPtr) $ fail $ "Undefined global: " ++ (show name)
         return val

toLLVMExp' m f b s e = trace ("Compiling expression " ++ show e) $ toLLVMExp m f b s e

toLLVMExp :: L.Module ->  -- Current module
             L.Value ->   -- Current function
             L.Builder -> -- IR Cursor
             [L.Value] -> -- De Bruijn levels
             SExp ->      -- Expression to process
             IO L.Value
toLLVMExp m f b s (SV v) = lookupVar m s v
-- TODO: Verify consistency of definition of tail call w/ LLVM
toLLVMExp m f b s (SApp isTail name vars)
    = do callee <- L.getNamedFunction m (show name)
         when (callee == nullPtr) $ fail $ "Undefined function: " ++ (show name)
         args <- mapM (lookupVar m s) vars
         call <- L.buildCall b callee args ""
         L.setTailCall call isTail
         return call
toLLVMExp m f b s (SLet name value body)
    = do v <- toLLVMExp m f b s value
         case name of
           Glob n -> L.setValueName v (show name)
           Loc _  -> return ()
         toLLVMExp m f b (s ++ [v]) body
toLLVMExp m f b s (SCon tag _ vars)
    = mapM (lookupVar m s) vars >>= buildCon m b tag
toLLVMExp m f b s (SCase var alts')
    = do let (alts, defaultAlt) =
                 foldl (\accum alt ->
                            case alt of
                              SDefaultCase exp -> (fst accum, Just alt)
                              _                -> (alt : fst accum, snd accum))
                       ([], Nothing) alts'
         let caseCount = case defaultAlt of
                           Just _  -> length alts - 1 -- Default case is treated specially
                           Nothing -> length alts
         value <- lookupVar m s var
         L.dumpValue value
         ctorPtr <- L.buildStructGEP b value 1 ""
         builtAlts <- mapM (buildAlt m f s ctorPtr) alts
         (defaultEntry, defaultExit, defaultVal) <-
             case defaultAlt of
               Just alt -> buildAlt m f s ctorPtr alt
               Nothing  -> do (block, val) <- buildCaseFail m f; return (block, block, val)
         switch <- case (head alts) of
                   SConCase _ _ _ _ _ ->
                       do tagPtr <- L.buildStructGEP b ctorPtr 0 ""
                          tag <- L.buildLoad b tagPtr "tag"
                          s <- L.buildSwitch b tag defaultEntry (fromIntegral caseCount)
                          mapM_ (uncurry $ L.addCase s)
                                $ map (\(SConCase _ ctorTag _ _ _, entry) ->
                                           (L.constInt L.int32Type (fromIntegral ctorTag) True, entry))
                                      $ zip alts $ map (\(entry, _, _) -> entry) builtAlts
                          return s
                   SConstCase (I _) _ ->
                       do intPtr <- L.buildPointerCast b ctorPtr L.int32Type ""
                          int <- L.buildLoad b intPtr ""
                          s <- L.buildSwitch b int defaultEntry (fromIntegral caseCount)
                          mapM_ (uncurry $ L.addCase s)
                                $ map (\(SConstCase (I i) _, entry) ->
                                           (L.constInt L.int32Type (fromIntegral i) True, entry))
                                      $ zip alts $ map (\(entry, _, _) -> entry) builtAlts
                          return s
         endBlock <- L.appendBasicBlock f "endCase"
         mapM_ (\(exitBlock, _, _) -> do
                  L.positionAtEnd b exitBlock
                  L.buildBr b endBlock) ((defaultEntry, defaultExit, defaultVal):builtAlts)
         L.positionAtEnd b endBlock
         vty <- idrValueTy m
         phi <- L.buildPhi b (L.pointerType vty 0) "caseResult"
         L.addIncoming phi $ map (\(_, exit, value) -> (value, exit)) builtAlts
         L.addIncoming phi [(defaultVal, defaultExit)]
         return phi
toLLVMExp m f b s (SConst const)
    = case const of
        I i   -> buildInt   m b $ L.constInt L.int32Type (fromIntegral i) True
        Fl f  -> buildFloat m b $ L.constReal L.doubleType $ realToFrac f
        Ch c  -> buildInt   m b $ L.constInt L.int32Type (fromIntegral $ fromEnum c) True
        Str s -> buildStr   m b $ L.constString s True
toLLVMExp m f b s (SForeign lang ftype name args)
    = case lang of
        LANG_C -> do ffun <- ensureBound m name ftype $ map fst args
                     argVals <- mapM (\(fty, v) -> do
                                        idrVal <- lookupVar m s v
                                        idrToNative b fty idrVal)
                                     args
                     L.buildCall b ffun argVals "" >>= cToIdr m b ftype
toLLVMExp m f b s (SOp prim vars)
    = do args <- mapM (lookupVar m s) vars
         case prim of
           LPlus -> binOp args FInt L.buildAdd
           LMinus -> binOp args FInt L.buildSub
           LTimes -> binOp args FInt L.buildMul
           LDiv -> binOp args FInt L.buildSDiv
           LFPlus -> binOp args FDouble L.buildFAdd
           LFMinus -> binOp args FDouble L.buildFSub
           LFTimes -> binOp args FDouble L.buildFMul
           LFDiv -> binOp args FDouble L.buildFDiv
           _ -> L.dumpModule m >> (fail $ "Unimplemented primitive operator: " ++ show prim)
    where
      binOp args ty f
          = do x <- idrToNative b ty $ args !! 0
               y <- idrToNative b ty $ args !! 1
               f b x y ""

