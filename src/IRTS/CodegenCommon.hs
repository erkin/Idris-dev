module IRTS.CodegenCommon where

import Core.TT
import IRTS.Simplified

data DbgLevel = NONE | DEBUG | TRACE deriving Eq
data OutputType = Raw | Object | Executable deriving (Eq, Show)

type Codegen
    = [(Name, SDecl)] -> -- Definitions
      String ->          -- output file name
      OutputType ->
      String ->          -- extra compiler flags
      DbgLevel ->
      IO ()
