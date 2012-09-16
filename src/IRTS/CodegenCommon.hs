module IRTS.CodegenCommon where

import Core.TT
import IRTS.Simplified

data DbgLevel = NONE | DEBUG | TRACE deriving Eq
data OutputType = Assembly | Object | Executable deriving Eq

type Codegen
    = [(Name, SDecl)] -> -- Definitions
      String ->          -- output file name
      OutputType ->
      [FilePath] ->      -- include files
      String ->          -- extra compiler flags
      DbgLevel ->
      IO ()
