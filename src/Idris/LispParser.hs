module Idris.LispParser where

import Data.Ratio
import Data.Maybe
import Data.List
import Text.ParserCombinators.Parsec
import Idris.ParserCommon
import Idris.AbsSyntax
import Core.TT

data SExpr = TSymbol [String]
           | TList [SExpr]
           | TInteger Integer
           | TRational Rational
           | TString String
           | TChar Char
            deriving (Show)

symbolName :: SExpr -> Maybe String
symbolName (TSymbol p) = Just $ last p
symbolName _ = Nothing

symbolPath :: SExpr -> Maybe [String]
symbolPath (TSymbol p) = Just p
symbolPath _ = Nothing

readSExprs :: String -> [SExpr]
readSExprs input = readSExprs' $ parse parseExprs "" input
    where readSExprs' (Left err) = error $ show err
          readSExprs' (Right result) = result

parseExprs :: GenParser Char a [SExpr]
parseExprs = do exprs <- sepEndBy exprOrComment whitespace
                eof
                return $ catMaybes exprs
             where
                exprOrComment :: GenParser Char a (Maybe SExpr)
                exprOrComment = whitespace >> ((parseLineComment >> return Nothing) <|> (parseExpr >>= return . Just))

parseExpr :: GenParser Char a SExpr
parseExpr = (fmap TList parseList <|> parseAtom)

commentChar :: Char
commentChar = ';'

namespaceChar :: Char
namespaceChar = '.'

parseLineComment :: GenParser Char a ()
parseLineComment =
    do char commentChar
       many (noneOf "\n")
       (newline >> return ()) <|> eof
       return ()

parseAtom :: GenParser Char a SExpr
parseAtom =     fmap TChar     parseChar
            <|> fmap TRational parseRational
            <|> fmap TInteger  parseInteger
            <|> fmap TString   parseString
            <|> fmap TSymbol   parseSymbol

parseSymbol :: GenParser Char a [String]
parseSymbol = sepBy1 (many1 symbolChar) (char namespaceChar)

parseRational = try parseFloat <|> try parseRatio
parseInteger = try parseHex <|> try parseDec

parseSign :: GenParser Char a Integer
parseSign = (char '+' >> return 1) <|> (char '-' >> return (-1)) <|> return 1

parseDec :: GenParser Char a Integer
parseDec = do sign <- parseSign
              digits <- many1 digit
              return $ sign * read digits

parseFloat :: GenParser Char a Rational
parseFloat = do sign <- parseSign
                whole <- many digit
                char '.'
                fraction <- many1 digit
                return $ (toRational sign) *
                         ((toRational $ if null whole then 0 else read whole) +
                                        (read fraction) % 10 ^ (length fraction))


parseRatio :: GenParser Char a Rational
parseRatio = do num <- parseInteger
                char '/'
                denom <- parseInteger
                return $ num % denom

parseHex :: GenParser Char a Integer
parseHex = do sign <- parseSign
              string "0x"
              digits <- many1 digit
              return $ sign * (read $ "0x" ++ digits)

parseChar :: GenParser Char a Char
parseChar = do char '\''
               x <- (escapedChar <|> noneOf "'")
               char '\''
               return x

parseList :: GenParser Char a [SExpr]
parseList = do char '('
               x <- many (whitespace >> parseExpr)
               whitespace
               char ')'
               return x

inList :: GenParser Char a b -> GenParser Char a b
inList p =
    do char '('
       x <- p
       char ')'
       return x


whitespaceChars = " \v\f\t\r\n"
whitespace = many $ oneOf whitespaceChars
symbolChar = noneOf $ commentChar : namespaceChar : "\"()" ++ whitespaceChars

parseString :: GenParser Char a String
parseString = do char '"'
                 s <- many (escapedChar <|> noneOf "\"\\")
                 char '"'
                 return s

escapedChar :: GenParser Char a Char
escapedChar = do char '\\'
                 c <- anyChar
                 return $ case c of
                               'n' -> '\n'
                               't' -> '\t'
                               _ -> c

-- Semantics
parseModuleDecl :: GenParser Char a (ModuleID, [ModuleID], String, SourcePos)
parseModuleDecl =
    do (TSymbol ["module"] : TSymbol name : xs) <- parseList
       rest <- getInput
       pos <- getPosition
       case xs of
         [TList (TSymbol ["import"] : importSyms)] ->
             return (name, mapMaybe symbolPath importSyms, rest, pos)
         [] -> return (name, [], rest, pos)
         _ -> fail $ "Not an import form: " ++ show xs

mkNS :: [String] -> Name
mkNS [x] = UN x
mkNS (x:xs) = NS (UN x) xs

parseDef :: SyntaxInfo -> IParser [PDecl]
parseDef syn = do
  (TSymbol ["def"] : TSymbol name : ty : args : body) <- parseList
  fc <- pfc
  let iname = mkNS (reverse name)
  return [PTy syn fc [] iname (mkTerm fc ty)]
--          PClauses fc [] iname [PClause fc iname ]]

mkTerm :: FC -> SExpr -> PTerm
mkTerm fc (TList (x:xs))     = PApp fc (mkTerm fc x) (map ((PExp 1 False) . (mkTerm fc)) $ xs)
mkTerm fc (TSymbol ["_"])    = Placeholder
mkTerm fc (TSymbol ['?':xs]) = PMetavar $ UN xs
mkTerm fc (TSymbol name)     = PRef fc (mkNS $ reverse name)
mkTerm _  (TString str)      = PConstant $ Str str
mkTerm _  (TChar char)       = PConstant $ Ch char
-- TODO: Prevent loss of precision here
mkTerm _  (TInteger i)       = PConstant $ I $ fromInteger i
mkTerm _  (TRational i)      = PConstant $ Fl $ fromRational i

importParser :: ImportParser
importParser =
    do i <- getInput
       p <- getPosition
       (try parseModuleDecl) <|> return ([], [], i, p)
