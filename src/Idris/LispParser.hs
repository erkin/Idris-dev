module Idris.LispParser where

import Data.Ratio
import Data.Maybe
import Data.List
import Text.ParserCombinators.Parsec
import Idris.ParserCommon
import Idris.AbsSyntax
import Core.TT

namespaceChar :: Char
namespaceChar = '.'

anySymbol :: GenParser Char a [String]
anySymbol = spaceOrComment >> sepBy1 (many1 symbolChar) (char namespaceChar)

symbol :: [String] -> GenParser Char s [String]
symbol n = spaceOrComment >> (string $ intercalate [namespaceChar] n) >> return n

lparen :: GenParser Char s ()
lparen = spaceOrComment >> char '(' >> return ()

rparen :: GenParser Char s ()
rparen = spaceOrComment >> char ')' >> return ()

listOf :: GenParser Char s a -> GenParser Char s a
listOf body = between lparen rparen body

commentChar :: Char
commentChar = ';'

parseLineComment :: GenParser Char a String
parseLineComment =
    (do char commentChar
        text <- many (noneOf "\n")
        (newline >> return ()) <|> eof
        return text) <?> "comment"

constant :: GenParser Char a PTerm
constant = do
  fc <- pfc
  spaceOrComment
  (    fmap (PConstant . Ch) charLiteral
   <|> fmap (PConstant . Str) stringLiteral
   -- TODO: Prevent loss of precision here
   <|> fmap (PConstant . Fl . fromRational) parseRational)
   <|> (parseInteger >>= \i -> return $ PApp fc (PRef fc (UN "fromInteger"))
                                             [pexp $ PConstant $ I $ fromInteger i])

parseRational = (try parseFloat <|> try parseRatio) <?> "rational literal"
parseInteger = (try hexLiteral <|> try parseDec) <?> "integer literal"

parseSign :: GenParser Char a Integer
parseSign = ((char '+' >> return 1) <|> (char '-' >> return (-1)) <|> return 1) <?> "sign"

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

hexLiteral :: GenParser Char a Integer
hexLiteral = do sign <- parseSign
                string "0x"
                digits <- many1 digit
                return $ sign * (read $ "0x" ++ digits)

charLiteral :: GenParser Char a Char
charLiteral = (do char '\''
                  x <- (escapedChar <|> noneOf "'")
                  char '\''
                  return x) <?> "character literal"

whitespaceChars = " \v\f\t\r\n"

spaceOrComment = many $ (oneOf whitespaceChars >> return ()) <|> (parseLineComment >> return ())

symbolChar = noneOf $ commentChar : namespaceChar : "\"()0123456789" ++ whitespaceChars

stringLiteral :: GenParser Char a String
stringLiteral = (do char '"'
                    s <- many (escapedChar <|> noneOf "\"\\")
                    char '"'
                    return s) <?> "string literal"

escapedChar :: GenParser Char a Char
escapedChar = do char '\\'
                 c <- anyChar
                 return $ case c of
                               'n' -> '\n'
                               't' -> '\t'
                               _ -> c

-- Semantics

parseModuleDecl :: GenParser Char a (ModuleID, [ModuleID], String, SourcePos)
parseModuleDecl = do
  (name, imports) <- listOf $ do
                       symbol ["module"]
                       name <- anySymbol
                       imports <- option [] $ listOf $ do
                                    symbol ["import"]
                                    many anySymbol
                       return (name, imports)
  rest <- getInput
  pos <- getPosition
  return (name, imports, rest, pos)

parseBody pos syn = do
  setPosition pos
  ds <- many (definition syn)
  eof
  i' <- getState
  return (concat ds, i')

mkNS :: [String] -> Name
mkNS [x] = UN x
mkNS (x:xs) = NS (UN x) xs

definition :: SyntaxInfo -> GenParser Char s [PDecl]
definition syn = do
  fc <- pfc
  (name, ty, args, body) <-
      listOf $ do
        symbol ["def"]
        name <- anySymbol
        ty <- expr
        args <- listOf (many anySymbol)
        body <- many expr
        return (name, ty, args, body)
  let iname = mkNS (reverse name)
      with = []
      whereblock = []
      applied = PApp fc (PRef fc iname) $
                map (pexp . (PRef fc)) (map (mkNS . reverse) args)
  return [PTy syn fc [] iname ty,
          PClauses fc [] iname [PClause fc iname applied with (last body) whereblock]]

-- TODO: Lexical lookup of special values
-- TODO: Avoid excessive use of try
expr :: GenParser Char s PTerm
expr = try constant
   <|> try metavar
   <|> (try $ do fc <- pfc; symbol ["_|_"]  >> return (PFalse fc))
   <|> (try $ do fc <- pfc; symbol ["unit"] >> return (PTrue fc))
   <|> (try $ do fc <- pfc; symbol ["refl"] >> return (PRefl fc))
   <|> (try                (symbol ["Set"]) >> return PSet)
   <|> (try                (symbol ["_"])   >> return Placeholder)
   <|> (try reference)
   <|> try eq
   <|> application

fullExpr :: GenParser Char s PTerm
fullExpr = do e <- expr
              spaceOrComment
              eof
              return e

eq :: GenParser Char s PTerm
eq = do
  fc <- pfc
  listOf $ do
       symbol ["="]
       x <- expr
       y <- expr
       return $ PEq fc x y

application :: GenParser Char s PTerm
application = do
  fc <- pfc
  listOf $ do
    f <- expr
    xs <- many expr
    return $ PApp fc f (map pexp xs)

metavar = spaceOrComment >> (fmap (PMetavar . UN . concat) $ char '?' >> anySymbol)

reference :: GenParser Char s PTerm
reference = do
  fc <- pfc
  name <- anySymbol
  return $ PRef fc (mkNS $ reverse name)

importParser :: ImportParser s
importParser =
    do i <- getInput
       p <- getPosition
       (try parseModuleDecl) <|> return ([], [], i, p)
