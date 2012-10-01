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
codegen defs out exec libs dbg
    = L.withModule "" $ \m -> do
        prims <- declarePrimitives m
        mapM_ (toLLVMDecl prims m . snd) defs
        mapM_ (toLLVMDef prims m . snd) defs
        error <- L.verifyModule m
        case error of
          Nothing  -> case exec of
                        Raw -> L.printModuleToFile m out
          Just msg -> L.dumpModule m >> fail msg

conTypeID = 0
intTypeID = 1
floatTypeID = 2
stringTypeID = 3
unitTypeID = 4
ptrTypeID = 5

data Prims = Prims { allocCon :: L.Value
                   , fprintf :: L.Value
                   , abort :: L.Value
                   , primReadStr :: L.Value
                   , primStdin :: L.Value
                   , primStdout :: L.Value
                   , primStderr :: L.Value
                   , primMkBigI :: L.Value
                   , primMkBigC :: L.Value
                   , primMkBigM :: L.Value
                   , primCastIntStr :: L.Value
                   , primCastStrInt :: L.Value
                   , primCastFloatStr :: L.Value
                   , primCastStrFloat :: L.Value
                   , primConcat :: L.Value
                   , primStrlt :: L.Value
                   , primStreq :: L.Value
                   , primStrlen :: L.Value
                   , primStrHead :: L.Value
                   , primStrTail :: L.Value
                   , primStrCons :: L.Value
                   , primStrIndex :: L.Value
                   , primStrRev :: L.Value
                   , primBigPlus :: L.Value
                   , primBigMinus :: L.Value
                   , primBigTimes :: L.Value
                   , primBigDivide :: L.Value
                   , primBigEq :: L.Value
                   , primBigLt :: L.Value
                   , primBigLe :: L.Value
                   , primBigGt :: L.Value
                   , primBigGe :: L.Value
                   , primCastIntBig :: L.Value
                   , primCastBigInt :: L.Value
                   , primCastStrBig :: L.Value
                   , primCastBigStr :: L.Value
                   , primFExp :: L.Value
                   , primFLog :: L.Value
                   , primFSin :: L.Value
                   , primFCos :: L.Value
                   , primFTan :: L.Value
                   , primFASin :: L.Value
                   , primFACos :: L.Value
                   , primFATan :: L.Value
                   , primFSqrt :: L.Value
                   , primFFloor :: L.Value
                   , primFCeil :: L.Value
                   , conTy :: L.Type
                   , valTy :: L.Type
                   }

declarePrimitives :: L.Module -> IO Prims
declarePrimitives m
    = do con <- L.structCreateNamed "constructor"
         val <- L.structCreateNamed "value"
         --                   Tag          Arity        Args
         L.structSetBody con [L.int32Type, L.int32Type, L.arrayType (L.pointerType val 0) 0] False
         --                   Type         Value
         L.structSetBody val [L.int32Type, con] False
         alloc <- L.addFunction m "allocCon"
                  $ L.functionType (L.pointerType val 0)
                        [L.pointerType L.int8Type 0, L.int32Type] False
         rstr <- L.addFunction m "idris_readStr"
                 $ L.functionType (L.pointerType val 0)
                       [L.pointerType L.int8Type 0, L.pointerType L.int8Type 0] False
         bigi <- L.addFunction m "MKBIGI"
                 $ L.functionType (L.pointerType val 0) [L.int32Type] False
         bigc <- L.addFunction m "MKBIGC"
                 $ L.functionType (L.pointerType val 0)
                       [L.pointerType L.int8Type 0, L.pointerType L.int8Type 0] False
         bigm <- L.addFunction m "MKBIGM"
                 $ L.functionType (L.pointerType val 0)
                       [L.pointerType L.int8Type 0, L.pointerType L.int8Type 0] False
         fpf <- L.addFunction m "fprintf"
                $ L.functionType L.int32Type [L.pointerType L.int8Type 0, L.pointerType L.int8Type 0] True
         abort' <- L.addFunction m "abort" $ L.functionType L.voidType [] False
         stdin'  <- L.addGlobal m (L.pointerType L.int8Type 0) "stdin"
         stdout' <- L.addGlobal m (L.pointerType L.int8Type 0) "stdout"
         stderr' <- L.addGlobal m (L.pointerType L.int8Type 0) "stderr"
         mapM_ (flip L.setLinkage L.ExternalLinkage) [stdin', stdout', stderr']
         let fpty = L.functionType L.doubleType [L.doubleType] False
         exp <- L.addFunction m "llvm.exp.f64" fpty
         log <- L.addFunction m "llvm.log.f64" fpty
         sin <- L.addFunction m "llvm.sin.f64" fpty
         cos <- L.addFunction m "llvm.cos.f64" fpty
         sqrt <- L.addFunction m "llvm.sqrt.f64" fpty
         floor <- L.addFunction m "llvm.floor.f64" fpty
         ceil <- L.addFunction m "ceil" fpty
         tan <- L.addFunction m "tan" fpty
         asin <- L.addFunction m "asin" fpty
         acos <- L.addFunction m "acos" fpty
         atan <- L.addFunction m "atan" fpty
         let bind name arity = L.addFunction m ("idris_" ++ name)
                               $ L.functionType (L.pointerType val 0)
                                     ((L.pointerType L.int8Type 0)
                                      : (replicate arity $ L.pointerType val 0)) False
         cis <- bind "castIntStr" 1
         csi <- bind "castStrInt" 1
         cfs <- bind "castFloatStr" 1
         csf <- bind "castStrFloat" 1
         cnc <- bind "concat" 2
         slt <- bind "strlt" 2
         seq <- bind "streq" 2
         sln <- bind "strlen" 2
         shd <- bind "strHead" 1
         stl <- bind "strTail" 1
         stc <- bind "strCons" 2
         sti <- bind "strIndex" 2
         str <- bind "strRev" 2
         bip <- bind "bigPlus" 2
         bim <- bind "bigMinus" 2
         bit <- bind "bigTimes" 2
         bid <- bind "bigDivide" 2
         beq <- bind "bigEq" 2
         blt <- bind "bigLt" 2
         ble <- bind "bigLe" 2
         bgt <- bind "bigGt" 2
         bge <- bind "bigGe" 2
         cib <- bind "castIntBig" 1
         cbi <- bind "castBigInt" 1
         csb <- bind "castStrBig" 1
         cbs <- bind "castBigStr" 1
         return $ Prims { allocCon = alloc
                        , fprintf = fpf
                        , abort = abort'
                        , primReadStr = rstr
                        , primStdin = stdin'
                        , primStdout = stdout'
                        , primStderr = stderr'
                        , primMkBigI = bigi
                        , primMkBigC = bigc
                        , primMkBigM = bigm
                        , primCastIntStr = cis
                        , primCastStrInt = csi
                        , primCastFloatStr = cfs
                        , primCastStrFloat = csf
                        , primConcat = cnc
                        , primStrlt = slt
                        , primStreq = seq
                        , primStrlen = sln
                        , primStrHead = shd
                        , primStrTail = stl
                        , primStrCons = stc
                        , primStrIndex = sti
                        , primStrRev = str
                        , primBigPlus = bip
                        , primBigMinus = bim
                        , primBigTimes = bit
                        , primBigDivide = bit
                        , primBigEq = beq
                        , primBigLt = blt
                        , primBigLe = ble
                        , primBigGt = bgt
                        , primBigGe = bge
                        , primCastIntBig = cib
                        , primCastBigInt = cbi
                        , primCastStrBig = csb
                        , primCastBigStr = cbs
                        , primFExp = exp
                        , primFLog = log
                        , primFSin = sin
                        , primFCos = cos
                        , primFTan = tan
                        , primFASin = asin
                        , primFACos = acos
                        , primFATan = atan
                        , primFSqrt = sqrt
                        , primFFloor = floor
                        , primFCeil = ceil
                        , conTy = con
                        , valTy = val
                        }

idrFuncTy :: Prims -> Int -> L.Type
idrFuncTy p n = L.functionType (L.pointerType (valTy p) 0)
                ((L.pointerType L.int8Type 0)
                 : (replicate n $ L.pointerType (valTy p) 0))
                False

llname :: Name -> String
llname n = "_idris_" ++ (show n)

toLLVMDecl :: Prims -> L.Module -> SDecl -> IO ()
toLLVMDecl p m (SFun name args _ _)
    = do f <- L.addFunction m (llname name) $ idrFuncTy p $ length args
         ps <- L.getParams f
         L.setValueName (head ps) "VM"
         mapM (uncurry L.setValueName) $ zip (tail ps) (map show args)
         return ()


toLLVMDef :: Prims -> L.Module -> SDecl -> IO ()
toLLVMDef prims m (SFun name args _ exp)
    = L.withBuilder $ \b -> do
        f <- fmap (maybe (error "toLLVMDef: impossible") id) $ L.getNamedFunction m (llname name)
        bb <- L.appendBasicBlock f "entry"
        L.positionAtEnd b bb
        params <- L.getParams f
        value <- toLLVMExp prims m f b (head params) (tail params) exp
        L.buildRet b value
        broken <- L.verifyFunction f
        when broken $ do
          L.dumpValue f
          err <- L.verifyModule m
          case err of
            Just msg -> fail $ "CodegenLLVM: Internal error: Broken function: " ++ msg
            Nothing -> error "CodegenLLVM.toLLVMDef: impossible"
        return ()

buildVal :: Prims -> L.Builder -> L.Value -> Int -> IO L.Value
buildVal prims b vm arity
    = L.buildCall b (allocCon prims) [vm, L.constInt L.int32Type (fromIntegral arity) True] ""

buildCon :: Prims -> L.Builder -> L.Value -> Int -> [L.Value] -> IO L.Value
buildCon prims b vm tag args
    = do val <- buildVal prims b vm $ length args
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
    = do shifted <- L.buildShl b value one ""
         tagged <- L.buildAdd b shifted one ""
         L.buildIntToPtr b tagged (L.pointerType (valTy prims) 0) ""
      where
        one = L.constInt L.int32Type 1 True

buildFloat :: Prims -> L.Builder -> L.Value -> L.Value -> IO L.Value
buildFloat prims b vm value
    = do val <- buildVal prims b vm 0
         typeid <- L.buildStructGEP b val 0 ""
         L.buildStore b (L.constInt L.int32Type floatTypeID True) typeid
         con <- L.buildStructGEP b val 1 ""
         floatPtr <- L.buildPointerCast b con (L.pointerType L.doubleType 0) ""
         L.buildStore b value floatPtr
         return val

buildPtr :: Prims -> L.Builder -> L.Value -> L.Value -> IO L.Value
buildPtr prims b vm value
    = do val <- buildVal prims b vm 0
         typeid <- L.buildStructGEP b val 0 ""
         L.buildStore b (L.constInt L.int32Type ptrTypeID True) typeid
         con <- L.buildStructGEP b val 1 ""
         ptrPtr <- L.buildPointerCast b con (L.pointerType (L.pointerType L.int8Type 0) 0) ""
         L.buildStore b value ptrPtr
         return val

buildStr :: Prims -> L.Builder -> L.Value -> L.Value -> IO L.Value
buildStr prims b vm value
    = do val <- buildVal prims b vm 0
         typeid <- L.buildStructGEP b val 0 ""
         L.buildStore b (L.constInt L.int32Type stringTypeID True) typeid
         con <- L.buildStructGEP b val 1 ""
         strPtr <- L.buildPointerCast b con (L.pointerType (L.pointerType L.int8Type 0) 0) ""
         L.buildStore b value strPtr
         return val

buildUnit :: Prims -> L.Builder -> L.Value -> IO L.Value
buildUnit p b vm
    = do val <- buildVal p b vm 0
         typeid <- L.buildStructGEP b val 0 ""
         L.buildStore b (L.constInt L.int32Type unitTypeID True) typeid
         return val

-- TODO: Runtime error
buildCaseFail :: Prims -> L.Value -> IO (L.BasicBlock, L.Value)
buildCaseFail prims f
    = do bb <- L.appendBasicBlock f "caseFail"
         return (bb, L.getUndef $ L.pointerType (valTy prims) 0)

buildAlt :: Prims -> L.Module -> L.Value -> L.Value -> [L.Value] -> L.Value -> SAlt ->
            IO (L.BasicBlock, L.BasicBlock, L.Value)
buildAlt p m f vm s _ (SDefaultCase body)
    = L.withBuilder $ \b -> do
        entry <- L.appendBasicBlock f "default"
        L.positionAtEnd b entry
        result <- toLLVMExp p m f b vm s body
        exit <- L.getInsertBlock b
        return (entry, exit, result)
buildAlt p m f vm s _ (SConstCase _ body)
    = L.withBuilder $ \b -> do
        entry <- L.appendBasicBlock f "alt"
        L.positionAtEnd b entry
        result <- toLLVMExp p m f b vm s body
        exit <- L.getInsertBlock b
        return (entry, exit, result)
buildAlt p m f vm s ctorPtr (SConCase _ _ _ argNames body)
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
        result <- toLLVMExp p m f b vm (s ++ args) body
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
idrToNative b ty v
    = case ty of
        FInt -> do shifted <- L.buildPtrToInt b v L.int32Type ""
                   L.buildAShr b shifted (L.constInt L.int32Type 1 True) ""
        _ -> do ctorPtr <- L.buildStructGEP b v 1 ""
                primPtr <- L.buildPointerCast b ctorPtr (L.pointerType (foreignToC ty) 0) ""
                L.buildLoad b primPtr ""

cToIdr :: Prims -> L.Builder -> FType -> L.Value -> L.Value -> IO L.Value
cToIdr prims b ty vm v
    = case ty of
        FInt  -> buildInt prims b v
        FChar -> buildInt prims b v
        FString -> buildStr   prims b vm v
        FDouble -> buildFloat prims b vm v
        FPtr    -> buildPtr   prims b vm v

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
             L.Value ->   -- VM pointer
             [L.Value] -> -- De Bruijn levels
             SExp ->      -- Expression to process
             IO L.Value
toLLVMExp p m f b vm s (SV v) = lookupVar m s v
-- TODO: Verify consistency of definition of tail call w/ LLVM
toLLVMExp p m f b vm s (SApp isTail name vars)
    = do maybeCallee <- L.getNamedFunction m (llname name)
         case maybeCallee of
           Nothing -> fail $ "Undefined function: " ++ (show name)
           Just callee -> do
             args <- mapM (lookupVar m s) vars
             call <- L.buildCall b callee (vm:args) ""
             L.setTailCall call isTail
             return call
toLLVMExp p m f b vm s (SLet name value body)
    = do v <- toLLVMExp p m f b vm s value
         case name of
           Glob n -> L.setValueName v (show name)
           Loc _  -> return ()
         toLLVMExp p m f b vm (s ++ [v]) body
toLLVMExp p m f b vm s (SCon tag _ vars)
    = mapM (lookupVar m s) vars >>= buildCon p b vm tag
toLLVMExp p m f b vm s (SCase var alts')
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
         builtAlts <- mapM (buildAlt p m f vm s ctorPtr) alts
         (defaultEntry, defaultExit, defaultVal) <-
             case defaultAlt of
               Just alt -> buildAlt p m f vm s ctorPtr alt
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
                       do int <- idrToNative b FInt value
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
toLLVMExp p m f b vm s (SConst const)
    = case const of
        I i   -> buildInt p b $ L.constInt L.int32Type (fromIntegral i) True
        BI i | i < (2^30) -> L.buildCall b (primMkBigI p) [L.constInt L.int32Type (fromIntegral i) True] ""
             | otherwise  -> do str <- L.buildGlobalStringPtr b (show i) "bigIntLiteral"
                                L.buildCall b (primMkBigC p) [vm, str] ""
        Fl f  -> buildFloat p b vm $ L.constReal L.doubleType $ realToFrac f
        Ch c  -> buildInt p b $ L.constInt L.int32Type (fromIntegral $ fromEnum c) True
        Str s -> L.buildGlobalStringPtr b s "stringLiteral" >>= buildStr p b vm
        _ -> return $ L.getUndef $ L.pointerType (valTy p) 0 -- Type values are undefined
toLLVMExp p m f b vm s (SForeign lang ftype name args)
    = case lang of
        LANG_C -> do ffun <- ensureBound m name ftype $ map fst args
                     argVals <- mapM (\(fty, v) -> do
                                        idrVal <- lookupVar m s v
                                        idrToNative b fty idrVal)
                                     args
                     L.buildCall b ffun argVals "" >>= cToIdr p b ftype vm
toLLVMExp p m f b vm s (SOp prim vars)
    = do args <- mapM (lookupVar m s) vars
         let icmp op = do x <- idrToNative b FInt $ args !! 0
                          y <- idrToNative b FInt $ args !! 1
                          v <- L.buildICmp b op x y ""
                          bool <- L.buildZExt b v (foreignToC FInt) ""
                          cToIdr p b FInt vm bool
             fcmp op = do x <- idrToNative b FDouble $ args !! 0
                          y <- idrToNative b FDouble $ args !! 1
                          v <- L.buildFCmp b op x y ""
                          bool <- L.buildZExt b v (foreignToC FInt) ""
                          cToIdr p b FInt vm bool
             binOp ty f = do x <- idrToNative b ty $ args !! 0
                             y <- idrToNative b ty $ args !! 1
                             v <- f b x y ""
                             cToIdr p b ty vm v
             binPr prim = L.buildCall b (prim p) [vm, args !! 0, args !! 1] ""
             unPr prim  = L.buildCall b (prim p) [vm, args !! 0] ""
             fpPr prim  = do x <- idrToNative b FDouble $ args !! 0
                             L.buildCall b (primFExp p) [x] ""
         case prim of
           LPlus  -> binOp FInt L.buildAdd
           LMinus -> binOp FInt L.buildSub
           LTimes -> binOp FInt L.buildMul
           LDiv   -> binOp FInt L.buildSDiv
           LEq -> icmp L.IntEQ
           LLt -> icmp L.IntSLT
           LLe -> icmp L.IntSLE
           LGt -> icmp L.IntSGT
           LGe -> icmp L.IntSGE
           LFPlus  -> binOp FDouble L.buildFAdd
           LFMinus -> binOp FDouble L.buildFSub
           LFTimes -> binOp FDouble L.buildFMul
           LFDiv   -> binOp FDouble L.buildFDiv
           LFEq -> fcmp L.FPOEQ
           LFLt -> fcmp L.FPOLT
           LFLe -> fcmp L.FPOLE
           LFGt -> fcmp L.FPOGT
           LFGe -> fcmp L.FPOGE
           LBPlus     -> binPr primBigPlus
           LBMinus    -> binPr primBigMinus
           LBTimes    -> binPr primBigTimes
           LBDiv      -> binPr primBigDivide
           LBEq       -> binPr primBigEq
           LBLt       -> binPr primBigLt
           LBLe       -> binPr primBigLe
           LBGt       -> binPr primBigGt
           LBGe       -> binPr primBigGe
           LStrConcat -> binPr primConcat
           LStrLt     -> binPr primStrlt
           LStrEq     -> binPr primStreq
           LStrLen    -> binPr primStrlen
           LIntStr    -> unPr primCastIntStr
           LStrInt    -> unPr primCastStrInt
           LFloatStr  -> unPr primCastFloatStr
           LStrFloat  -> unPr primCastStrFloat
           LIntBig    -> unPr primCastIntBig
           LBigInt    -> unPr primCastBigInt
           LStrBig    -> unPr primCastStrBig
           LBigStr    -> unPr primCastBigStr
           LStrHead   -> unPr primStrHead
           LStrTail   -> unPr primStrTail
           LStrCons   -> unPr primStrCons
           LStrIndex  -> unPr primStrIndex
           LStrRev    -> unPr primStrRev
           LFExp      -> fpPr primFExp
           LFLog      -> fpPr primFLog
           LFSin      -> fpPr primFSin
           LFCos      -> fpPr primFCos
           LFTan      -> fpPr primFTan
           LFASin     -> fpPr primFASin
           LFACos     -> fpPr primFACos
           LFATan     -> fpPr primFATan
           LFSqrt     -> fpPr primFSqrt
           LFFloor    -> fpPr primFFloor
           LFCeil     -> fpPr primFCeil
           LPrintNum -> do fmt <- L.buildGlobalStringPtr b "%d" ""
                           num <- idrToNative b FInt $ args !! 0
                           stdout <- L.buildLoad b (primStdout p) ""
                           L.buildCall b (fprintf p) [stdout, fmt, num] ""
                           buildUnit p b vm
           LPrintStr -> do fmt <- L.buildGlobalStringPtr b "%s" ""
                           str <- idrToNative b FString $ args !! 0
                           stdout <- L.buildLoad b (primStdout p) ""
                           L.buildCall b (fprintf p) [stdout, fmt, str] ""
                           buildUnit p b vm
           LReadStr -> do ptrPtr <- L.buildStructGEP b (args !! 0) 1 ""
                          ptr <- idrToNative b FPtr $ args !! 0
                          L.buildCall b (primReadStr p) [vm, ptr] ""
           LIntFloat ->
               do x <- idrToNative b FInt $ args !! 0
                  f <- L.buildSIToFP b x L.doubleType ""
                  buildFloat p b vm f
           LFloatInt ->
               do x <- idrToNative b FDouble $ args !! 0
                  f <- L.buildFPToSI b x L.int32Type ""
                  buildFloat p b vm f
           LNoOp -> return $ L.getUndef $ L.pointerType (valTy p) 0
           _ -> L.dumpModule m >> (fail $ "Unimplemented primitive operator: " ++ show prim)
toLLVMExp p m f b vm s (SError str)
    = do fmt <- L.buildGlobalStringPtr b "%s" ""
         str <- L.buildGlobalStringPtr b str "stringLiteral"
         stderr <- L.buildLoad b (primStderr p) ""
         L.buildCall b (fprintf p) [stderr, fmt, str] ""
         L.buildCall b (abort p) [] ""
         return $ L.getUndef $ L.pointerType (valTy p) 0
