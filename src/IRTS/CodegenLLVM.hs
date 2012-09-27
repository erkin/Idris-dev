{-# LANGUAGE TypeOperators #-}
module IRTS.CodegenLLVM (codegen) where

import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.CodegenCommon
import Core.TT

import qualified LLVM.Wrapper.Core as L
import qualified LLVM.Wrapper.BitWriter as L
import qualified LLVM.Wrapper.Analysis as L

import Foreign.Ptr
import Control.Monad

codegen :: Codegen
codegen defs out exec incs libs dbg
    = L.withModule "" $ \m -> do
        prims <- declarePrimitives m
        mapM_ (toLLVMDecl prims m . snd) defs
        mapM_ (toLLVMDef prims m . snd) defs
        error <- L.verifyModule m
        case error of
          Nothing  -> L.writeBitcodeToFile m out
          Just msg -> fail msg

conTypeID = 0
intTypeID = 1
floatTypeID = 2
stringTypeID = 3
unitTypeID = 4
ptrTypeID = 5

data Prims = Prims { alloc   :: L.Value
                   , strlen  :: L.Value
                   , strcpy  :: L.Value
                   , strcat  :: L.Value
                   , sprintf :: L.Value
                   , printf  :: L.Value
                   , conTy   :: L.Type
                   , valTy   :: L.Type
                   }

declarePrimitives :: L.Module -> IO Prims
declarePrimitives m -- TODO: GC
    = do al <- L.addFunction m "malloc" $ L.functionType (L.pointerType L.int8Type 0)
                                          [L.int64Type] False
         slen <- L.addFunction m "strlen" $ L.functionType L.int64Type [L.pointerType L.int8Type 0] False
         cpy <- L.addFunction m "strcpy" $ L.functionType (L.pointerType L.int8Type 0)
                                           [L.pointerType L.int8Type 0, L.pointerType L.int8Type 0] False
         cat <- L.addFunction m "strcat" $ L.functionType (L.pointerType L.int8Type 0)
                                           [L.pointerType L.int8Type 0, L.pointerType L.int8Type 0] False
         spf <- L.addFunction m "sprintf" $ L.functionType L.int32Type [ L.pointerType L.int8Type 0
                                                                       , L.pointerType L.int8Type 0] True
         pf <- L.addFunction m "sprintf" $ L.functionType L.int32Type [L.pointerType L.int8Type 0] True
         con <- L.structCreateNamed "constructor"
         val <- L.structCreateNamed "value"
         -- TODO: Is arity actually needed here?
         --                     Tag          Arity        Args
         L.structSetBody con [L.int32Type, L.int32Type, L.arrayType (L.pointerType val 0) 0] False
         --                     Type         Value
         L.structSetBody val [L.int32Type, con] False
         return $ Prims al slen cpy cat spf pf con val

idrFuncTy :: Prims -> Int -> L.Type
idrFuncTy p n = L.functionType (L.pointerType (valTy p) 0) (replicate n $ L.pointerType (valTy p) 0) False

llname :: Name -> String
llname n = "_idris_" ++ (show n)

toLLVMDecl :: Prims -> L.Module -> SDecl -> IO ()
toLLVMDecl p m (SFun name args _ _)
    = do f <- L.addFunction m (llname name) $ idrFuncTy p $ length args
         ps <- L.getParams f
         mapM (uncurry L.setValueName) $ zip ps (map show args)
         return ()


toLLVMDef :: Prims -> L.Module -> SDecl -> IO ()
toLLVMDef prims m (SFun name args _ exp)
    = L.withBuilder $ \b -> do
        f <- fmap (maybe (error "toLLVMDef: impossible") id) $ L.getNamedFunction m (llname name)
        bb <- L.appendBasicBlock f "entry"
        L.positionAtEnd b bb
        params <- L.getParams f
        value <- toLLVMExp prims m f b params exp
        L.buildRet b value
        return ()

buildAlloc :: Prims -> L.Builder -> L.Value -> IO L.Value
buildAlloc prims b bytes
    = L.buildCall b (alloc prims) [bytes] ""

buildVal :: Prims -> L.Builder -> Int -> IO L.Value
buildVal prims b argCount
    = do valSize <- L.sizeOf (valTy prims)
         ptrSize <- L.sizeOf (L.pointerType L.int8Type 0)
         mem <- buildAlloc prims b (L.constAdd valSize
                                     (L.constMul ptrSize
                                           (L.constInt L.int64Type
                                                 (fromIntegral $ argCount) True)))
         L.buildPointerCast b mem (L.pointerType (valTy prims) 0) ""

buildCon :: Prims -> L.Builder -> Int -> [L.Value] -> IO L.Value
buildCon prims b tag args
    = do val <- buildVal prims b $ length args
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

buildInt :: Prims -> L.Builder -> L.Value -> IO L.Value
buildInt prims b value
    = do val <- buildVal prims b 0
         con <- L.buildStructGEP b val 1 ""
         intPtr <- L.buildPointerCast b con (L.pointerType L.int32Type 0) ""
         L.buildStore b value intPtr
         return val

buildFloat :: Prims -> L.Builder -> L.Value -> IO L.Value
buildFloat prims b value
    = do val <- buildVal prims b 0
         con <- L.buildStructGEP b val 1 ""
         floatPtr <- L.buildPointerCast b con (L.pointerType L.doubleType 0) ""
         L.buildStore b value floatPtr
         return val

buildStr :: Prims -> L.Builder -> L.Value -> IO L.Value
buildStr prims b value
    = do val <- buildVal prims b 0
         con <- L.buildStructGEP b val 1 ""
         strPtr <- L.buildPointerCast b con (L.pointerType (L.pointerType L.int8Type 0) 0) ""
         L.buildStore b value strPtr
         return val

-- TODO: Runtime error
buildCaseFail :: Prims -> L.Value -> IO (L.BasicBlock, L.Value)
buildCaseFail prims f
    = do bb <- L.appendBasicBlock f "caseFail"
         return (bb, L.getUndef $ L.pointerType (valTy prims) 0)

buildAlt :: Prims -> L.Module -> L.Value -> [L.Value] -> L.Value -> SAlt ->
            IO (L.BasicBlock, L.BasicBlock, L.Value)
buildAlt p m f s _ (SDefaultCase body)
    = L.withBuilder $ \b -> do
        entry <- L.appendBasicBlock f "default"
        L.positionAtEnd b entry
        result <- toLLVMExp p m f b s body
        exit <- L.getInsertBlock b
        return (entry, exit, result)
buildAlt p m f s ctorPtr (SConCase _ _ _ argNames body)
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
        result <- toLLVMExp p m f b (s ++ args) body
        exit <- L.getInsertBlock b
        return (entry, exit, result)
buildAlt p m f s _ (SConstCase _ body)
    = L.withBuilder $ \b -> do
        entry <- L.appendBasicBlock f "alt"
        L.positionAtEnd b entry
        result <- toLLVMExp p m f b s body
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

cToIdr :: Prims -> L.Builder -> FType -> L.Value -> IO L.Value
cToIdr prims b ty v
    = case ty of
        FInt -> buildInt prims b v
        FString -> buildStr prims b v
        FDouble -> buildFloat prims b v

ensureBound :: L.Module -> String -> FType -> [FType] -> IO L.Value
ensureBound m name rty argtys
    = do maybef <- L.getNamedFunction m name
         case maybef of
           Nothing -> L.addFunction m name $ L.functionType (foreignToC rty) (map foreignToC argtys) False
           Just f -> return f

lookupVar :: L.Module -> [L.Value] -> LVar -> IO L.Value
lookupVar m s (Loc level) = return $ s !! level
lookupVar m s (Glob name)
    = do maybeVal <- L.getNamedGlobal m (show name)
         case maybeVal of
           Nothing -> fail $ "Undefined global: " ++ (show name)
           Just val -> return val

toLLVMExp :: Prims ->
             L.Module ->  -- Current module
             L.Value ->   -- Current function
             L.Builder -> -- IR Cursor
             [L.Value] -> -- De Bruijn levels
             SExp ->      -- Expression to process
             IO L.Value
toLLVMExp p m f b s (SV v) = lookupVar m s v
-- TODO: Verify consistency of definition of tail call w/ LLVM
toLLVMExp p m f b s (SApp isTail name vars)
    = do maybeCallee <- L.getNamedFunction m (llname name)
         case maybeCallee of
           Nothing -> fail $ "Undefined function: " ++ (show name)
           Just callee -> do
             args <- mapM (lookupVar m s) vars
             call <- L.buildCall b callee args ""
             L.setTailCall call isTail
             return call
toLLVMExp p m f b s (SLet name value body)
    = do v <- toLLVMExp p m f b s value
         case name of
           Glob n -> L.setValueName v (show name)
           Loc _  -> return ()
         toLLVMExp p m f b (s ++ [v]) body
toLLVMExp p m f b s (SCon tag _ vars)
    = mapM (lookupVar m s) vars >>= buildCon p b tag
toLLVMExp p m f b s (SCase var alts')
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
         ctorPtr <- L.buildStructGEP b value 1 ""
         builtAlts <- mapM (buildAlt p m f s ctorPtr) alts
         (defaultEntry, defaultExit, defaultVal) <-
             case defaultAlt of
               Just alt -> buildAlt p m f s ctorPtr alt
               Nothing  -> do (block, val) <- buildCaseFail p f; return (block, block, val)
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
                       do intPtr <- L.buildPointerCast b ctorPtr (L.pointerType L.int32Type 0) ""
                          int <- L.buildLoad b intPtr ""
                          s <- L.buildSwitch b int defaultEntry (fromIntegral caseCount)
                          mapM_ (uncurry $ L.addCase s)
                                $ map (\(SConstCase (I i) _, entry) ->
                                           (L.constInt L.int32Type (fromIntegral i) True, entry))
                                      $ zip alts $ map (\(entry, _, _) -> entry) builtAlts
                          return s
         endBlock <- L.appendBasicBlock f "endCase"
         mapM_ (\(_, exitBlock, _) -> do
                  L.positionAtEnd b exitBlock
                  L.buildBr b endBlock) ((defaultEntry, defaultExit, defaultVal):builtAlts)
         L.positionAtEnd b endBlock
         phi <- L.buildPhi b (L.pointerType (valTy p) 0) "caseResult"
         L.addIncoming phi $ map (\(_, exit, value) -> (value, exit)) builtAlts
         L.addIncoming phi [(defaultVal, defaultExit)]
         return phi
toLLVMExp p m f b s (SConst const)
    = case const of
        I i   -> buildInt   p b $ L.constInt L.int32Type (fromIntegral i) True
        Fl f  -> buildFloat p b $ L.constReal L.doubleType $ realToFrac f
        Ch c  -> buildInt   p b $ L.constInt L.int32Type (fromIntegral $ fromEnum c) True
        Str s -> L.buildGlobalStringPtr b s "" >>= buildStr p b
toLLVMExp p m f b s (SForeign lang ftype name args)
    = case lang of
        LANG_C -> do ffun <- ensureBound m name ftype $ map fst args
                     argVals <- mapM (\(fty, v) -> do
                                        idrVal <- lookupVar m s v
                                        idrToNative b fty idrVal)
                                     args
                     L.buildCall b ffun argVals "" >>= cToIdr p b ftype
toLLVMExp p m f b s (SOp prim vars)
    = do args <- mapM (lookupVar m s) vars
         case prim of
           LPlus -> binOp args FInt L.buildAdd
           LMinus -> binOp args FInt L.buildSub
           LTimes -> binOp args FInt L.buildMul
           LDiv -> binOp args FInt L.buildSDiv
           LEq -> icmp args L.IntEQ
           LLt -> icmp args L.IntSLT
           LLe -> icmp args L.IntSLE
           LGt -> icmp args L.IntSGT
           LGe -> icmp args L.IntSGE
           LFPlus -> binOp args FDouble L.buildFAdd
           LFMinus -> binOp args FDouble L.buildFSub
           LFTimes -> binOp args FDouble L.buildFMul
           LFDiv -> binOp args FDouble L.buildFDiv
           LFEq -> fcmp args L.FPOEQ
           LFLt -> fcmp args L.FPOLT
           LFLe -> fcmp args L.FPOLE
           LFGt -> fcmp args L.FPOGT
           LFGe -> fcmp args L.FPOGE
           LStrConcat ->
               do x <- idrToNative b FString $ args !! 0
                  y <- idrToNative b FString $ args !! 1
                  xlen <- L.buildCall b (strlen p) [x] ""
                  ylen <- L.buildCall b (strlen p) [y] ""
                  sum <- L.buildAdd b xlen ylen ""
                  destlen <- L.buildAdd b sum (L.constInt L.int64Type 1 True) ""
                  mem <- buildAlloc p b destlen
                  L.buildCall b (strcpy p) [mem, x] ""
                  L.buildCall b (strcat p) [mem, y] ""
                  buildStr p b mem
           LStrLen ->
               do x <- idrToNative b FString $ args !! 0
                  len <- L.buildCall b (strlen p) [x] ""
                  i32 <- L.buildTrunc b len L.int32Type ""
                  buildInt p b i32
           LIntFloat ->
               do x <- idrToNative b FInt $ args !! 0
                  f <- L.buildSIToFP b x L.doubleType ""
                  buildFloat p b f
           LFloatInt ->
               do x <- idrToNative b FInt $ args !! 0
                  f <- L.buildFPToSI b x L.int32Type ""
                  buildFloat p b f
           LIntStr -> -- 2^31 is 10 digits, so 10 chars + 1 null byte
               do mem <- buildAlloc p b $ L.constInt L.int64Type 11 True
                  arg <- idrToNative b FInt $ args !! 0
                  fmt <- L.buildGlobalStringPtr b "%d" ""
                  L.buildCall b (sprintf p) [mem, fmt, arg] ""
                  buildStr p b mem
           LFloatStr -> -- Edwin used 32 here
               do mem <- buildAlloc p b $ L.constInt L.int64Type 32 True
                  arg <- idrToNative b FDouble $ args !! 0
                  fmt <- L.buildGlobalStringPtr b "%f" ""
                  L.buildCall b (sprintf p) [mem, fmt, arg] ""
                  buildStr p b mem
           LPrintStr ->
               do arg <- idrToNative b FString $ args !! 0
                  buildPrint "%s" [arg]
           LPrintNum ->
               do arg <- idrToNative b FInt $ args !! 0
                  buildPrint "%d" [arg]
           _ -> L.dumpModule m >> (fail $ "Unimplemented primitive operator: " ++ show prim)
    where
      buildPrint fmt args
          = do fmtptr <- L.buildGlobalStringPtr b fmt ""
               r <- L.buildCall b (printf p) (fmtptr:args) ""
               buildInt p b r
      icmp args op
          = do x <- idrToNative b FInt $ args !! 0
               y <- idrToNative b FInt $ args !! 1
               v <- L.buildICmp b op x y ""
               bool <- L.buildZExt b v (foreignToC FInt) ""
               cToIdr p b FInt bool
      fcmp args op
          = do x <- idrToNative b FDouble $ args !! 0
               y <- idrToNative b FDouble $ args !! 1
               v <- L.buildFCmp b op x y ""
               bool <- L.buildZExt b v (foreignToC FInt) ""
               cToIdr p b FInt bool
      binOp args ty f
          = do x <- idrToNative b ty $ args !! 0
               y <- idrToNative b ty $ args !! 1
               v <- f b x y ""
               cToIdr p b ty v
