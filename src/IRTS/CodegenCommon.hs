module IRTS.CodegenCommon where

data DbgLevel = NONE | DEBUG | TRACE deriving Eq
data OutputType = Assembly | Object | Executable deriving Eq
