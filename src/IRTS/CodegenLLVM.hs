{-# LANGUAGE TypeOperators #-}
module IRTS.CodegenLLVM where

import IRTS.Bytecode
import IRTS.Lang
import IRTS.LParser
import IRTS.Simplified
import IRTS.CodegenCommon
import Core.TT

import qualified LLVM.Wrapper.Core as L
import qualified LLVM.Wrapper.BitWriter as L

import Data.Maybe
import Foreign.Ptr

fovm :: FilePath -> IO ()
fovm f = do defs <- parseFOVM f
            codegenLLVM defs "a.out" Executable ["math.h"] "" TRACE

codegenLLVM :: [(Name, LDecl)] ->
            String -> -- output file name
            OutputType ->   -- generate executable if True, only .o if False 
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
                        declarePrimitives m
                        mapM_ (toLLVMDecl m . snd) defs
                        --mapM_ (toLLVMDef m . snd) defs
                        L.writeBitcodeToFile m out
           Error e -> fail $ "Can't happen: Something went wrong in codegenLLVM\n" ++ show e

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

toLLVMDecl :: L.Module -> LDecl -> IO ()
toLLVMDecl m (LConstructor _ _ _) = return ()
toLLVMDecl m (LFun name args def)
    = do ty <- idrFuncTy m $ length args
         f <- L.addFunction m (show name) ty
         ps <- L.getParams f
         mapM (uncurry L.setValueName) $ zip ps (map show args)
         return ()


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

buildAlloc :: L.Module -> L.Builder -> L.Value -> IO L.Value
buildAlloc m b bytes
    = do malloc <- L.getNamedFunction m "malloc" -- TODO: GC
         L.buildCall b malloc [bytes] "memory"

buildVal :: L.Module -> L.Builder -> Int -> IO L.Value
buildVal m b argCount
    = do valSize <- idrValueTy m >>= L.sizeOf
         ptrSize <- L.sizeOf (L.pointerType L.int8Type 0)
         mem <- buildAlloc m b (L.constAdd valSize
                                     (L.constMul ptrSize
                                           (L.constInt L.int32Type
                                                 (fromIntegral $ argCount) True)))
         destTy <- idrValueTy m >>= \t -> return $ L.pointerType t 0
         L.buildBitCast b mem destTy "value"

buildCon :: L.Module -> L.Builder -> Int -> [L.Value] -> IO L.Value
buildCon m b tag args
    = do val <- buildVal m b $ length args
         valTyPtr <- L.buildStructGEP b val 0 "typePtr"
         L.buildStore b (L.constInt L.int32Type conTypeID True) valTyPtr
         conPtr <- L.buildStructGEP b val 1 "constructorPtr"
         tagPtr <- L.buildStructGEP b conPtr 0 "tagPtr"
         L.buildStore b (L.constInt L.int32Type (fromIntegral tag) True) tagPtr
         arityPtr <- L.buildStructGEP b conPtr 1 "arityPtr"
         L.buildStore b arityPtr (L.constInt L.int32Type (fromIntegral $ length args) True)
         argArray <- L.buildStructGEP b conPtr 2 "argArrayPtr"
         mapM (\(arg, idx) -> do
                 place <- L.buildInBoundsGEP b argArray
                          [L.constInt L.int32Type (fromIntegral $ length args) True] $
                          "arg" ++ (show idx)
                 L.buildStore b arg place) $ zip args [0..]
         return val

buildInt :: L.Module -> L.Builder -> L.Value -> IO L.Value
buildInt m b value
    = do val <- buildVal m b 0
         con <- L.buildStructGEP b val 1 ""
         intPtr <- L.buildBitCast b con L.int32Type "intPtr"
         L.buildStore b value intPtr
         return val

buildFloat :: L.Module -> L.Builder -> L.Value -> IO L.Value
buildFloat m b value
    = do val <- buildVal m b 0
         con <- L.buildStructGEP b val 1 ""
         floatPtr <- L.buildBitCast b con L.floatType "floatPtr"
         L.buildStore b value floatPtr
         return val

buildStr :: L.Module -> L.Builder -> L.Value -> IO L.Value
buildStr m b value
    = do val <- buildVal m b 0
         con <- L.buildStructGEP b val 1 ""
         strPtr <- L.buildBitCast b con (L.pointerType L.int8Type 0) "strPtr"
         L.buildStore b value strPtr
         return val

-- TODO: Runtime error
-- TODO: Compiletime warning
buildCaseFailBlock :: L.Module -> L.BasicBlock -> L.Value -> IO (L.BasicBlock, L.Value)
buildCaseFailBlock m end f
    = L.withBuilder $ \b -> do
        bb <- L.appendBasicBlock f "caseFail"
        L.positionAtEnd b bb
        L.buildBr b end
        vt <- idrValueTy m
        return (bb, L.getUndef vt)

buildCaseBlock endBlock m f s e
    = L.withBuilder $ \b -> do
        bb <- L.appendBasicBlock f "case" -- TODO: Better name
        L.positionAtEnd b bb
        value <- toLLVMExp m f b s e
        L.buildBr b endBlock
        caseExitBlock <- L.getInsertBlock b
        return (caseExitBlock, value)

foreignToC :: FType -> L.Type
foreignToC ty = case ty of
                  FInt -> L.int32Type
                  FString -> L.pointerType L.int8Type 0
                  FUnit -> L.voidType
                  FPtr -> L.pointerType L.int8Type 0
                  FDouble -> L.doubleType

idrToNative :: L.Builder -> FType -> L.Value -> IO L.Value
idrToNative b ty v = do ctorPtr <- L.buildStructGEP b v 1 ""
                        primPtr <- L.buildBitCast b ctorPtr
                                   (L.pointerType (foreignToC ty) 0) "primPtr"
                        L.buildLoad b primPtr "foreignValue"

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

toLLVMExp :: L.Module ->  -- Current module
             L.Value ->   -- Current function
             L.Builder -> -- IR Cursor
             [L.Value] -> -- De Bruijn levels
             LExp ->      -- Expression to process
             IO L.Value
toLLVMExp m f b s (LV var)
    = case var of
        Loc level -> return $ s !! level
        Glob name -> L.getNamedGlobal m (show name)
-- TODO: Verify consistency of definition of tail call w/ LLVM
toLLVMExp m f b s (LApp isTail name exps)
    = do callee <- L.getNamedFunction m (show name)
         args <- mapM (toLLVMExp m f b s) exps
         call <- L.buildCall b callee args ""
         L.setTailCall call isTail
         return call
toLLVMExp m f b s (LLet name value body)
    = do v <- toLLVMExp m f b s value
         toLLVMExp m f b (s ++ [v]) body
toLLVMExp m f b s (LCon tag _ exps)
    = mapM (toLLVMExp m f b s) exps >>= buildCon m b tag
toLLVMExp m f b s (LCase exp alts)
    = do let (cases, defaultAlt) =
                 foldl (\accum alt ->
                            case alt of
                              LDefaultCase exp -> (fst accum, Just exp)
                              _                -> (alt : fst accum, snd accum))
                       ([], Nothing) alts
         value <- toLLVMExp m f b s exp
         endBlock <- L.appendBasicBlock f "caseEnd"
         (defaultCase, defaultVal) <- maybe (buildCaseFailBlock m endBlock f)
                                            (buildCaseBlock endBlock m f s) defaultAlt
         case any (\alt -> case alt of
                             LConCase _ _ _ _ -> False
                             LConstCase _ _ -> True) cases of
           -- Constructor case
           False -> do ctor <- L.buildStructGEP b value 1 "constructorPtr"
                       tagPtr <- L.buildStructGEP b ctor 0 "tagPtr"
                       tag <- L.buildLoad b tagPtr "tag"
                       switch <- L.buildSwitch b tag defaultCase $ fromIntegral $
                                 case defaultAlt of
                                   Just _  -> 1 - length cases
                                   Nothing -> length cases
                       argArray <- L.buildStructGEP b ctor 2 "argArrayPtr"
                       results <-
                           mapM (\alt -> case alt of
                                           LConCase tag _ vars body -> do
                                             bindings <-
                                                 mapM (\(name, idx) -> do
                                                         L.buildInBoundsGEP b argArray
                                                              [L.constInt L.int32Type (fromIntegral tag) True]
                                                              (show name))
                                                      $ zip vars [0..]
                                             (block, value) <-
                                                 buildCaseBlock endBlock m f (s ++ bindings) body
                                             L.addCase switch
                                                       (L.constInt L.int32Type (fromIntegral tag) True)
                                                       block
                                             return (block, value)) cases
                       L.positionAtEnd b endBlock
                       vt <- idrValueTy m
                       phi <- L.buildPhi b vt "caseResult"
                       L.addIncoming phi results
                       return phi
           -- Constant case
           True -> undefined
toLLVMExp m f b s (LConst const)
    = case const of
        I i   -> buildInt   m b $ L.constInt L.int32Type (fromIntegral i) True
        Fl f  -> buildFloat m b $ L.constReal L.floatType $ realToFrac f
        Ch c  -> buildInt   m b $ L.constInt L.int32Type (fromIntegral $ fromEnum c) True
        Str s -> buildStr   m b $ L.constString s True
toLLVMExp m f b s (LForeign lang ftype name args)
    = case lang of
        LANG_C -> do ffun <- ensureBound m name ftype $ map fst args
                     argVals <- mapM (\(fty, e) -> do
                                        idrVal <- toLLVMExp m f b s e
                                        idrToNative b fty idrVal)
                                     args
                     L.buildCall b ffun argVals "foreignVal" >>= cToIdr m b ftype
toLLVMExp m f b s (LOp prim exps)
    = do args <- mapM (toLLVMExp m f b s) exps
         case prim of
           LPlus -> binOp args FInt L.buildAdd
           LMinus -> binOp args FInt L.buildSub
           LTimes -> binOp args FInt L.buildMul
           LDiv -> binOp args FInt L.buildSDiv
           LFPlus -> binOp args FDouble L.buildFAdd
           LFMinus -> binOp args FDouble L.buildFSub
           LFTimes -> binOp args FDouble L.buildFMul
           LFDiv -> binOp args FDouble L.buildFDiv
    where
      binOp args ty f
          = do x <- idrToNative b ty $ args !! 0
               y <- idrToNative b ty $ args !! 1
               f b x y ""

