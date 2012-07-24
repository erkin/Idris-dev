module Idris.LispParser where

import Data.Ratio
import Data.Maybe
import Data.List
import Text.ParserCombinators.Parsec
import Idris.ParserCommon

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
parseExprs = do exprs <- many exprOrComment
                spaces
                eof
                return $ catMaybes exprs
             where
                exprOrComment :: GenParser Char a (Maybe SExpr)
                exprOrComment = spaces >> ((parseLineComment >> return Nothing) <|> (parseExpr >>= return . Just))

parseExpr :: GenParser Char a SExpr
parseExpr = (parseList <|> parseAtom)

commentChar :: Char
commentChar = ';'

namespaceChar :: Char
namespaceChar = '.'

parseLineComment :: GenParser Char a ()
parseLineComment = char commentChar >> anyChar >> ((newline >> return ())  <|> eof) >> return ()

parseAtom :: GenParser Char a SExpr
parseAtom = parseChar <|> parseNumber <|> parseString <|> parseSymbol

parseSymbol :: GenParser Char a SExpr
parseSymbol = do cs <- sepBy (many symbolChar) (char namespaceChar)
                 return $ TSymbol $ cs

parseNumber :: GenParser Char a SExpr
parseNumber =   try (parseHex     >>= return . TInteger)
            <|> try (parseFloat   >>= return . TRational)
            <|> try (parseRatio   >>= return . TRational)
            <|> try (parseInteger >>= return . TInteger)

parseSign :: GenParser Char a Integer
parseSign = (char '+' >> return 1) <|> (char '-' >> return (-1)) <|> return 1

parseInteger :: GenParser Char a Integer
parseInteger = do sign <- parseSign
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

parseChar :: GenParser Char a SExpr
parseChar = do char '\''
               x <- (escapedChar <|> noneOf "'")
               char '\''
               return $ TChar x

parseList :: GenParser Char a SExpr
parseList = do char '('
               x <- sepBy parseExpr separator
               char ')'
               return $ TList x

inList :: GenParser Char a b -> GenParser Char a b
inList p =
    do char '('
       x <- p
       char ')'
       return x


symbolChar :: GenParser Char a Char
symbolChar = noneOf $ commentChar : namespaceChar : "() \v\f\t\r\n"

separator = skipMany1 space

parseString :: GenParser Char a SExpr
parseString = do char '"'
                 s <- many (escapedChar <|> noneOf "\"\\")
                 char '"'
                 return $ TString s

escapedChar :: GenParser Char a Char
escapedChar = do char '\\'
                 c <- anyChar
                 return $ case c of
                               'n' -> '\n'
                               't' -> '\t'
                               _ -> c

-- Semantics
parseModuleDecl :: GenParser Char a (ModuleID, [ModuleID], String, SourcePos)
parseModuleDecl = try $
    do TList (TSymbol ["module"] : TSymbol name : xs) <- parseList
       rest <- getInput
       pos <- getPosition
       case xs of
         [TList (TSymbol ["import"] : importSyms)] ->
             return (name, mapMaybe symbolPath importSyms, rest, pos)
         [] -> return (name, [], rest, pos)
         _ -> fail $ "Not an import form: " ++ show xs


importParser :: ImportParser
importParser =
    do i <- getInput
       p <- getPosition
       parseModuleDecl <|> return ([], [], i, p)
