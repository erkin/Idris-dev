module Idris.ParserCommon where

import Idris.AbsSyntax
import Idris.DSL
import Idris.Imports
import Idris.Error
import Idris.ElabDecls
import Idris.ElabTerm
import Idris.Coverage
import Idris.IBC
import Idris.Unlit

import Core.CoreParser
import Core.TT
import Core.Evaluate

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as PTok

import Data.List
import Control.Monad.State
import Debug.Trace
import Data.Maybe
import System.FilePath

type ModuleID = [String]
type IParser = GenParser Char IState
type ProgParser = SourcePos -> SyntaxInfo -> IParser ([PDecl], IState)
--                           module    imports     code    start
type ImportParser = IParser (ModuleID, [ModuleID], String, SourcePos)

-- Loading modules

loadModule :: FilePath -> ImportParser -> ProgParser -> Idris String
loadModule f ip pp
   = idrisCatch (do i <- getIState
                    ibcsd <- valIBCSubDir i
                    ids <- allImportDirs i
                    fp <- liftIO $ findImport ids ibcsd f
                    if (f `elem` imported i)
                       then iLOG $ "Already read " ++ f
                       else do putIState (i { imported = f : imported i })
                               case fp of
                                   IDR fn  -> loadSource False fn ip pp
                                   LIDR fn -> loadSource True  fn ip pp
                                   IBC fn src -> 
                                     idrisCatch (loadIBC fn)
                                                (\c -> do iLOG $ fn ++ " failed " ++ show c
                                                          case src of
                                                            IDR sfn -> loadSource False sfn ip pp
                                                            LIDR sfn -> loadSource True sfn ip pp)
                    let (dir, fh) = splitFileName f
                    return (dropExtension fh))
                (\e -> do let msg = show e
                          setErrLine (getErrLine msg)
                          iputStrLn msg
                          return "")

loadSource :: Bool -> FilePath -> ImportParser -> ProgParser -> Idris () 
loadSource lidr f importParser progParser
             = do iLOG ("Reading " ++ f)
                  file_in <- liftIO $ readFile f
                  file <- if lidr then tclift $ unlit f file_in else return file_in
                  (mname, modules, rest, pos) <- parseImports f file importParser
                  let modulePaths = map (intercalate "/") modules
                  i <- getIState
                  putIState (i { default_access = Hidden })
                  mapM_ (\m -> loadModule m importParser progParser) modulePaths
                  clearIBC -- start a new .ibc file
                  mapM_ (\m -> addIBC (IBCImport m)) modulePaths
                  ds' <- parseProg (defaultSyntax {syn_namespace = reverse mname }) 
                                   f rest pos progParser
                  let ds = namespaces mname ds'
                  logLvl 3 (dumpDecls ds)
                  i <- getIState
                  logLvl 10 (show (toAlist (idris_implicits i)))
                  logLvl 3 (show (idris_infixes i))
                  -- Now add all the declarations to the context
                  v <- verbose
                  when v $ iputStrLn $ "Type checking " ++ f
                  mapM_ (elabDecl toplevel) ds
                  i <- get
                  mapM_ checkDeclTotality (idris_totcheck i)
                  iLOG ("Finished " ++ f)
                  ibcsd <- valIBCSubDir i
                  let ibc = ibcPathNoFallback ibcsd f
                  iucheck
                  i <- getIState
                  addHides (hide_list i)
                  ok <- noErrors
                  when ok $
                    idrisCatch (do writeIBC f ibc; clearIBC)
                               (\c -> return ()) -- failure is harmless
                  i <- getIState
                  putIState (i { hide_list = [] })
                  return ()
  where
    namespaces []     ds = ds
    namespaces (x:xs) ds = [PNamespace x (namespaces xs ds)]

addHides :: [(Name, Maybe Accessibility)] -> Idris ()
addHides xs = do i <- getIState
                 let defh = default_access i
                 let (hs, as) = partition isNothing xs
                 if null as then return ()
                            else mapM_ doHide
                                    (map (\ (n, _) -> (n, defh)) hs ++
                                     map (\ (n, Just a) -> (n, a)) as)
  where isNothing (_, Nothing) = True
        isNothing _            = False

        doHide (n, a) = do setAccessibility n a
                           addIBC (IBCAccess n a)

parseImports :: FilePath -> String -> ImportParser -> Idris (ModuleID, [ModuleID], String, SourcePos)
parseImports fname input parser
    = do i <- get
         case (runParser parser i fname input) of
            Left err -> fail (show err)
            Right x -> do return x
  where ishow err = let ln = sourceLine (errorPos err) in
                        fname ++ ":" ++ show ln ++ ":parse error"
--                            ++ show (map messageString (errorMessages err))

parseProg :: SyntaxInfo -> FilePath -> String -> SourcePos -> ProgParser -> Idris [PDecl]
parseProg syn fname input pos parser
    = do i <- get
         case (runParser (parser pos syn) i fname input) of
            Left err -> fail (ishow err)
            Right (x, i) -> do put i
                               return (collect x)
  where ishow err = let ln = sourceLine (errorPos err) in
                        fname ++ ":" ++ show ln ++ ":parse error"
                              ++ " at column " ++ show (sourceColumn (errorPos err))
--                           show (map messageString (errorMessages err))

-- Collect PClauses with the same function name

collect :: [PDecl] -> [PDecl]
collect (c@(PClauses _ o _ _) : ds) 
    = clauses (cname c) [] (c : ds)
  where clauses n acc (PClauses fc _ _ [PClause fc' n' l ws r w] : ds)
           | n == n' = clauses n (PClause fc' n' l ws r (collect w) : acc) ds
        clauses n acc (PClauses fc _ _ [PWith fc' n' l ws r w] : ds)
           | n == n' = clauses n (PWith fc' n' l ws r (collect w) : acc) ds
        clauses n acc xs = PClauses (getfc c) o n (reverse acc) : collect xs

        cname (PClauses fc _ _ [PClause _ n _ _ _ _]) = n
        cname (PClauses fc _ _ [PWith   _ n _ _ _ _]) = n
        getfc (PClauses fc _ _ _) = fc

collect (PParams f ns ps : ds) = PParams f ns (collect ps) : collect ds
collect (PNamespace ns ps : ds) = PNamespace ns (collect ps) : collect ds
collect (PClass f s cs n ps ds : ds') = PClass f s cs n ps (collect ds) : collect ds'
collect (PInstance f s cs n ps t en ds : ds') 
    = PInstance f s cs n ps t en (collect ds) : collect ds'
collect (d : ds) = d : collect ds
collect [] = []
