module Idris.ParserCommon where

import Text.ParserCombinators.Parsec
import Idris.AbsSyntax
import Core.TT
import System.FilePath


type ModuleID = [String]
type IParser = GenParser Char IState
type ProgParser = SourcePos -> SyntaxInfo -> IParser ([PDecl], IState)
--                           module    imports     code    start
type ImportParser = IParser (ModuleID, [ModuleID], String, SourcePos)

pfc :: GenParser a s FC
pfc = do s <- getPosition
         let (dir, file) = splitFileName (sourceName s)
         let f = case dir of
                    "./" -> file
                    _ -> sourceName s
         return $ FC f (sourceLine s)
