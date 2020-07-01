{-# LANGUAGE OverloadedStrings #-}

module Ariel.Syntax.Parse
  ( runParseExpr,
    runParseDecl,
    runParseReplStmt,
  )
where

import Ariel.Syntax.AST
import Ariel.Syntax.Types
import Control.Applicative
import Control.Monad (when)
import Control.Monad.State.Strict (State, evalState, get, put)
import Data.Char (isSymbol)
import Data.Functor (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.Vector as V
import Data.Void
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

defaultOperatorInfo :: OperatorInfo
defaultOperatorInfo =
  OperatorInfo
    { assoc = InfixL,
      prec = 9
    }

type Operators = Map Name OperatorInfo

type Parser = P.ParsecT Void Text (State Operators)

-- Helper function to insert values in a Map
-- and also check if the value was already inserted
insertLookup :: Ord k => k -> v -> Map k v -> (Maybe v, Map k v)
insertLookup key value m =
  let replaceStrategy _ _ old = old -- if the key is alread in the map, keep the old value
   in M.insertLookupWithKey replaceStrategy key value m

-- Register a new operator.
-- In case the operator has already been registered, an error is issued
registerOperator :: Name -> OperatorInfo -> Parser ()
registerOperator name info = do
  (oldOp, newMap) <- insertLookup name info <$> get
  -- Check for duplicates
  case oldOp of
    -- TODO: probably it's better to use a custom error
    Just _ -> fail "Duplicate operator registration"
    Nothing -> put newMap

lookupOperator :: Name -> Parser OperatorInfo
lookupOperator name = do
  op <- M.lookup name <$> get
  case op of
    Nothing -> return defaultOperatorInfo
    Just info -> return info

-- Lexical structure of Ariel
singleLineComment :: Parser ()
singleLineComment = L.skipLineComment "//"

multiLineComment :: Parser ()
multiLineComment = L.skipBlockCommentNested "/*" "*/"

ignoreSpaceAndComents :: Parser ()
ignoreSpaceAndComents = L.space P.space1 singleLineComment multiLineComment

spaceLike :: Parser ()
spaceLike = L.space (void $ P.oneOf [' ', '\t']) empty empty

-- Parse a lexeme and ignore spaces and comments
lexeme :: Parser a -> Parser a
lexeme = L.lexeme ignoreSpaceAndComents

-- Parse a symbol and ignore spaces and comments
symbol :: Text -> Parser ()
symbol = void . L.symbol ignoreSpaceAndComents

keyword :: Text -> Parser ()
keyword w = P.string w *> P.notFollowedBy P.alphaNumChar *> spaceLike

integer :: Parser Int
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

text :: Parser Text
text = lexeme $ do
  _ <- P.char '"'
  str <- P.manyTill L.charLiteral (P.char '"')
  return $ T.pack str

identifier :: Parser Name
identifier = lexeme $ normalIdentifier <|> quotedIdentifier <|> symbolIdentifier
  where
    normalIdentifier = Name <$> P.some P.letterChar
    symbolIdentifier = Name <$> P.some (P.satisfy (\c -> isSymbol c && c /= '#'))
    quotedIdentifier = do
      _ <- P.char '\''
      Name <$> P.someTill L.charLiteral (P.char '\'')

-- Parser definition

-- Helper functions
betweenParens :: Parser a -> Parser a
betweenParens = P.between (symbol "(") (symbol ")")

betweenBraces :: Parser a -> Parser a
betweenBraces = P.between (symbol "{") (symbol "}")

-- Declarations

-- EBNF:
-- ArgList ::= '(' (identifier ',')* ')'
-- TermBinding    ::= identifier '=' Expr
--                  | identifier ArgList '=' Expr
--
-- Fixity ::= 'infixl' | 'infixr' | 'infix'
-- OperatorDecl ::= 'operator' '(' identifier ',' Fixity ',' integer ')'
--
-- Decl ::= TermBinding
--        | OperatorDecl

parseDecl :: Parser Decl
parseDecl =
  P.choice
    [ TermBinding <$> parseTermBinding,
      uncurry OperatorDecl <$> parseOperatorDecl
    ]

parseNameDecl :: Name -> Parser TermDecl
parseNameDecl name = do
  symbol "="
  body <- parseExpr
  return $ TermDecl name body

parseArgList :: Parser [Name]
parseArgList =
  betweenParens $ P.sepBy identifier (symbol ",")

parseFunDecl :: Name -> Parser TermDecl
parseFunDecl name = do
  args <- parseArgList
  symbol "="
  expr <- parseExpr
  return $ TermDecl name (variadicLambda args expr)

parseTermBinding :: Parser TermDecl
parseTermBinding = do
  name <- identifier
  termBinding <- parseNameDecl name <|> parseFunDecl name
  return termBinding

-- Once an operator decl has been parsed,
-- the operator is added to the operators table of the parser
parseOperatorDecl :: Parser (Name, OperatorInfo)
parseOperatorDecl = do
  keyword "operator"
  symbol "("
  opName <- identifier
  symbol ","
  opAssoc <- parseAssociativity
  symbol ","
  opPrec <- integer
  symbol ")"
  -- Check that the precedence is correct
  when (opPrec < 0 || opPrec > 9) $ fail "Invalid precedence value, it must be an integer between 0 and 9"
  let opInfo = OperatorInfo opAssoc opPrec
  -- Register operator in the op table
  registerOperator opName opInfo
  return (opName, opInfo)

parseAssociativity :: Parser Assoc
parseAssociativity =
  P.choice
    [ InfixL <$ P.string "infixl",
      InfixR <$ P.string "infixr",
      InfixN <$ P.string "infixn"
    ]

-- Expressions

-- EBNF:
-- PrimaryExpr ::= LetExpr
--               | CaseExpr
--               | PrimExpr
--               | ConExpr
--               | LetRecExpr
--               | LambdaExpr
--               | identifier
--               | Literal
--               | '(' Expr ')'
--               | Tuple
--
-- ExprList ::= '(' (Expr ',')* ')'
--
-- Term ::= PrimaryExpr (ExprList)*
--
-- Expr ::= Expr identifier Expr
--        | Term
--
-- Tuple ::= '{' (PrimaryExpr ',')* '}'
--
-- LetExpr ::= 'let' Decl ',' Expr
-- LetRecExpr ::= 'let' 'rec' Decl ',' Expr
-- CaseExpr ::= '#case' ExprList
-- PrimExpr ::= '#prim' identifier ExprList
-- ConExpr ::= '#con' integer ExprList

parseExpr :: Parser Expr
parseExpr =
  let noOp = OperatorInfo InfixN (-1)
   in parseExpr' noOp

-- Precedence Parser

-- Given two operators determine the precedence rules
data PrecRule
  = LeftBindsTighter
  | RightBindsTighter
  | CannotMix

comparePrec :: OperatorInfo -> OperatorInfo -> PrecRule
comparePrec left right = case compare (prec left) (prec right) of
  LT -> LeftBindsTighter
  GT -> RightBindsTighter
  EQ ->
    if (assoc left) /= (assoc right)
      then CannotMix
      else case assoc left of
        InfixL -> LeftBindsTighter
        InfixR -> RightBindsTighter
        InfixN -> CannotMix

parseExpr' :: OperatorInfo -> Parser Expr
parseExpr' currOpInfo = parseTerm >>= parseExprRest currOpInfo

-- TODO: This function looks quite a lot like a fold,
-- probably there is a clearer way to state it
parseExprRest :: OperatorInfo -> Expr -> Parser Expr
parseExprRest currOpInfo lhs = go <|> pure lhs
  where
    go :: Parser Expr
    go = do
      -- peek the next operator
      newOp <- P.lookAhead identifier
      newOpInfo <- lookupOperator newOp
      case currOpInfo `comparePrec` newOpInfo of
        CannotMix -> fail "Cannot mix operators"
        LeftBindsTighter -> return lhs
        RightBindsTighter -> do
          -- consume the operator
          _ <- identifier
          rhs <- parseExpr' newOpInfo
          let newLhs = App (App (Var newOp) lhs) rhs
          -- Loop again
          parseExprRest currOpInfo newLhs

parseExprArgList :: Parser [Expr]
parseExprArgList =
  betweenParens $ P.sepBy parseExpr (symbol ",")

parseTerm :: Parser Expr
parseTerm = do
  primary <- parsePrimary
  argLists <- many parseExprArgList
  -- Function application is left associative
  pure $ variadicApply primary (concat argLists)

parsePrimary :: Parser Expr
parsePrimary =
  P.choice
    [ parseLetRec,
      parseLet,
      parsePrimitive,
      parseCoreCase,
      parseCoreConstructor,
      parsePrimaryStartingWithIdent,
      parsePrimaryStartingWithParen,
      parseLiteral,
      parseTuple
    ]

-- non recursive let expression
parseLet :: Parser Expr
parseLet = do
  keyword "let"
  (TermDecl name binding) <- parseTermBinding
  symbol ","
  expr <- parseExpr
  return $ Let name binding expr

-- recursive let expression
parseLetRec :: Parser Expr
parseLetRec = do
  -- try to parse let rec, if not don't consume
  -- otherwise parseLet can't parse a regular let statement anymore
  P.try $ keyword "let" *> keyword "rec"
  (TermDecl name binding) <- parseTermBinding
  symbol ","
  expr <- parseExpr
  return $ LetRec name binding expr

parseCoreCase :: Parser Expr
parseCoreCase = do
  keyword "#case"
  e <- parseExpr
  cases <- parseExprArgList
  return $ CoreCase e (V.fromList cases)

parsePrimitive :: Parser Expr
parsePrimitive = do
  keyword "#prim"
  name <- identifier
  args <- parseExprArgList
  case args of
    [a1] -> return $ Prim1 name a1
    [a1, a2] -> return $ Prim2 name a1 a2
    _ -> fail "Wrong number of arguments for primitive"

parseCoreConstructor :: Parser Expr
parseCoreConstructor = do
  keyword "#con"
  index <- integer
  when (index < 0) $ fail "The index must be a non negative integer"
  args <- parseExprArgList
  return $ CoreCons (ConsIx index) args

parsePrimaryStartingWithIdent :: Parser Expr
parsePrimaryStartingWithIdent = do
  ident <- identifier
  P.choice
    [ parseSingleArgLambda ident,
      pure (Var ident)
    ]

parseSingleArgLambda :: Name -> Parser Expr
parseSingleArgLambda arg = do
  symbol "=>"
  expr <- parseExpr
  return $ Lam arg expr

-- TODO: Try removing this 'try'
parsePrimaryStartingWithParen :: Parser Expr
parsePrimaryStartingWithParen = parseMultiArgLambda <|> betweenParens parseExpr
  where
    parseMultiArgLambda = do
      -- Wrap the first part in a try, because there can be
      -- common prefixes between parseMultiArgLambda
      -- and a parenthesized expression, but once we find a =>
      -- we are sure that what's coming ahead must be a lambda expression
      args <- P.try $ parseArgList <* symbol "=>"
      expr <- parseExpr
      return $ variadicLambda args expr

parseLiteral :: Parser Expr
parseLiteral =
  P.choice
    [ Double <$> P.try float,
      Int <$> integer,
      Text <$> text
    ]

-- Parse tuples of form '{' (PrimaryExpr, )* '}'
parseTuple :: Parser Expr
parseTuple = do
  exprs <- betweenBraces $ P.sepBy parsePrimary (symbol ",")
  return $ Tuple $ V.fromList exprs

-- Run parser and in case of error pretty print the error message
runParser :: Parser a -> Text -> Either String a
runParser p input =
  let parser = ignoreSpaceAndComents >> p
      opTable = M.empty
      result = evalState (P.runParserT parser "" input) opTable
   in case result of
        Left err -> Left (P.errorBundlePretty err)
        Right res -> Right res

-- Public interface
runParseExpr :: Text -> Either String Expr
runParseExpr = runParser parseExpr

runParseDecl :: Text -> Either String Decl
runParseDecl = runParser parseDecl

runParseReplStmt :: Text -> Either String ReplStmt
runParseReplStmt =
  let parser =
        P.choice
          [ Decl <$> P.try parseDecl,
            Expr <$> parseExpr
          ]
   in runParser parser
