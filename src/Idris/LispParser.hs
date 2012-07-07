module Idris.LispParser where

import Control.Monad
import Text.ParserCombinators.Parsec

data SExpr = TSymbol String
           | TList [SExpr]
           | TInteger Integer
           | TString String
           | TChar Char
            deriving (Show)

readSExprs :: String -> [SExpr]
readSExprs input = readSExprs' $ parse parseExprs "" input
    where readSExprs' (Left err) = error $ show err
          readSExprs' (Right result) = result

parseExprs :: Parser [SExpr]
parseExprs = do exprs <- many parseExpr
                eof
                return exprs

parseExpr :: Parser SExpr
parseExpr = spaces >> (parseList <|> parseAtom)

parseAtom :: Parser SExpr
parseAtom = parseChar <|> try parseHex <|> try parseInteger <|> parseString <|> parseSymbol

parseSymbol :: Parser SExpr
parseSymbol = do first <- symbolChar
                 rest <- many (symbolChar <|> digit)
                 return $ TSymbol $ first:rest

parseSign :: Parser Integer
parseSign = (char '+' >> return 1) <|> (char '-' >> return (-1)) <|> return 1

parseInteger :: Parser SExpr
parseInteger = do sign <- parseSign
                  digits <- many1 digit
                  return $ TInteger $ sign * (read digits)

parseHex :: Parser SExpr
parseHex = do sign <- parseSign
              string "0x"
              digits <- many1 digit
              return $ TInteger $ sign * (read $ "0x" ++ digits)

parseChar :: Parser SExpr
parseChar = do char '\''
               x <- anyChar
               char '\''
               return $ TChar x

parseList :: Parser SExpr
parseList = do char '('
               x <- sepBy parseExpr separator
               char ')'
               return $ TList x

symbolChar :: Parser Char
symbolChar = noneOf "() \v\f\t\r\n"

separator = skipMany1 space

parseString :: Parser SExpr
parseString = do char '"'
                 s <- many (escapedChar <|> noneOf "\"\\")
                 char '"'
                 return $ TString s

escapedChar :: Parser Char
escapedChar = do char '\\'
                 c <- oneOf "\"\\ntu"
                 return $ case c of
                               '"' -> '"'
                               'n' -> '\n'
                               't' -> '\t'
                               '\\' -> '\\'
