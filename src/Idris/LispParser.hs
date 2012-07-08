module Idris.LispParser where

import Data.Ratio
import Text.ParserCombinators.Parsec

data SExpr = TSymbol String
           | TList [SExpr]
           | TInteger Integer
           | TRational Rational
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
parseAtom = parseChar <|> parseNumber <|> parseString <|> parseSymbol

parseSymbol :: Parser SExpr
parseSymbol = do first <- symbolChar
                 rest <- many (symbolChar <|> digit)
                 return $ TSymbol $ first:rest

parseNumber :: Parser SExpr
parseNumber = try parseHex <|> try parseFloat <|> try parseRatio <|> try parseInteger

parseSign :: Parser Integer
parseSign = (char '+' >> return 1) <|> (char '-' >> return (-1)) <|> return 1

parseInteger :: Parser SExpr
parseInteger = do sign <- parseSign
                  digits <- many1 digit
                  return $ TInteger $ sign * read digits

parseFloat :: Parser SExpr
parseFloat = do sign <- parseSign
                whole <- many digit
                char '.'
                fraction <- many1 digit
                return $ TRational $ (toRational sign) *
                                     ((toRational $ if null whole then 0 else read whole) +
                                      (read fraction) % 10 ^ (length fraction))

parseRatio :: Parser SExpr
parseRatio = do numSign <- parseSign
                num <- many1 digit
                char '/'
                denomSign <- parseSign
                denom <- many1 digit
                return $ TRational $ (numSign * read num) % (denomSign * read denom)

parseHex :: Parser SExpr
parseHex = do sign <- parseSign
              string "0x"
              digits <- many1 digit
              return $ TInteger $ sign * (read $ "0x" ++ digits)

parseChar :: Parser SExpr
parseChar = do char '\''
               x <- (escapedChar <|> noneOf "'")
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
                 c <- anyChar
                 return $ case c of
                               'n' -> '\n'
                               't' -> '\t'
                               _ -> c
