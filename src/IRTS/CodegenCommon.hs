{-# LANGUAGE CPP #-}

module IRTS.CodegenCommon where

import Core.TT
import IRTS.Simplified

import System.IO
import System.Environment

data DbgLevel = NONE | DEBUG | TRACE deriving Eq
data OutputType = Raw | Object | Executable deriving (Eq, Show)

type Codegen
    = [(Name, SDecl)] -> -- Definitions
      String ->          -- output file name
      OutputType ->
      String ->          -- extra compiler flags
      DbgLevel ->
      IO ()

tempfile :: IO (FilePath, Handle)
tempfile = do env <- environment "TMPDIR"
              let dir = case env of
                              Nothing -> "/tmp"
                              (Just d) -> d
              openTempFile dir "idris"

environment :: String -> IO (Maybe String)
environment x = catch (do e <- getEnv x
                          return (Just e))
#if MIN_VERSION_base(4,6,0)
                          (\y-> do return (y::SomeException);  return Nothing)
#endif
#if !MIN_VERSION_base(4,6,0)
                          (\_->  return Nothing)
#endif
