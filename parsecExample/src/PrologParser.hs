module PrologParser where

import           Control.Monad                          ()
import           Data.Either                            (fromLeft)
import           PrologAst                              (Atom (Atom),
                                                         Identifier,
                                                         PrologProgram (Program),
                                                         Relation (Relation),
                                                         RelationBody (..),
                                                         Type (..),
                                                         TypeDef (..), Var)
import           System.IO                              ()
import           Text.ParserCombinators.Parsec          (ParseError, Parser,
                                                         alphaNum, char, eof,
                                                         lower, many,
                                                         optionMaybe, parse,
                                                         sepBy, sepBy1, spaces,
                                                         upper, (<|>))
import           Text.ParserCombinators.Parsec.Expr     ()
import           Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token    as Token

languageDef =
  emptyDef
    { Token.identStart = lower
    , Token.identLetter = alphaNum <|> char '_'
    , Token.reservedNames = ["module", "type"]
    , Token.reservedOpNames = [",", ";", "->", ":-"]
    }

lexer = Token.makeTokenParser languageDef

identifier :: Parser Identifier
identifier = Token.identifier lexer

var :: Parser Var
var = do
  h <- upper
  t <- many (alphaNum <|> char '_')
  spaces
  return (h : t)

whiteSpace = Token.whiteSpace lexer

reservedOp = Token.reservedOp lexer

reserved = Token.reserved lexer

brackets = Token.parens lexer

squares = Token.squares lexer

dot = Token.dot lexer

atom :: Parser Atom
atom =
  list <|>
  (do id <- identifier
      list <- many bracedAtom
      return $ Atom id list)
  where
    bracedAtom :: Parser (Either Atom Var)
    bracedAtom =
      fmap Left list <|> fmap Right var <|>
      fmap (Left . flip Atom []) identifier <|>
      brackets maybeBracedAtom
    maybeBracedAtom :: Parser (Either Atom Var)
    maybeBracedAtom =
      fmap Right var <|> fmap Left atom <|> brackets maybeBracedAtom

maybeRelationBody :: Parser (Maybe RelationBody)
maybeRelationBody = optionMaybe (reservedOp ":-" >> disjunction)
  where
    disjunction :: Parser RelationBody
    disjunction = do
      x <- sepBy1 conjunction (reservedOp ";")
      return $ foldr1 Disj x
    conjunction :: Parser RelationBody
    conjunction = do
      x <- sepBy1 factor (reservedOp ",")
      return $ foldr1 Conj x
    factor :: Parser RelationBody
    factor = brackets disjunction <|> fmap RAtom atom

relation :: Parser Relation
relation = do
  a <- atom
  relB <- maybeRelationBody
  dot
  return $ Relation a relB

parseModule :: Parser Identifier
parseModule = do
  reserved "module"
  id <- identifier
  dot
  return id

typeExpr :: Parser Type
typeExpr = typeList
  where
    typeList :: Parser Type
    typeList = do
      x <- sepBy1 factor (reservedOp "->")
      return $ foldr1 Arrow x
    factor :: Parser Type
    factor = fmap Var var <|> fmap TAtom atom <|> brackets typeList

typ :: Parser TypeDef
typ = do
  reserved "type"
  id <- identifier
  ty <- typeExpr
  dot
  return $ TypeDef id ty

list :: Parser Atom
list = squares unsquaredList
  where
    unsquaredList :: Parser Atom
    unsquaredList = do
      x <- sepBy factor (reservedOp ",")
      case x of
        [] -> return $ Atom "nil" []
        x  -> notEmptyList x
    notEmptyList :: [Either Atom Var] -> Parser Atom
    notEmptyList x = do
      ender <- optionMaybe (reservedOp "|" >> var)
      case ender of
        Just end -> build x (Right end)
        Nothing  -> build x nil
    factor :: Parser (Either Atom Var)
    factor = fmap Left list <|> fmap Right var <|> fmap Left atom
    build :: [Either Atom Var] -> Either Atom Var -> Parser Atom
    build x end =
      return $
      fromLeft undefined $ foldr (\a b -> Left $ Atom "cons" [a, b]) end x
    nil :: Either Atom Var
    nil = Left (Atom "nil" [])

prog :: Parser PrologProgram
prog = do
  spaces
  id <- optionMaybe parseModule
  ty <- many typ
  re <- many relation
  eof
  return $ Program id ty re

parseText :: String -> Either ParseError PrologProgram
parseText = parse prog ""
