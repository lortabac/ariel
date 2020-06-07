{-# LANGUAGE OverloadedStrings #-}
module Ariel.Syntax.Parse
( runParseExpr
, runParseDecl
, runParseReplStmt
)
where
    
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Void
import Data.Functor (void)
import Control.Applicative
import Control.Monad.State.Strict (State, evalState, get, put)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Ariel.Syntax.AST
import Ariel.Syntax.Types

data Assoc = InfixL | InfixR | InfixN
           deriving(Eq)

data OperatorInfo = OperatorInfo
                  { assoc :: Assoc
                  , prec  :: Int
                  }

defaultOperatorInfo :: OperatorInfo
defaultOperatorInfo = OperatorInfo
                    { assoc = InfixL
                    , prec = 9
                    }

type Operators = Map Name OperatorInfo

type Parser = P.ParsecT Void Text (State Operators)

-- Helper function to insert values in a Map
-- and also check if the value was already inserted
insertLookup :: Ord k => k -> v -> Map k v -> (Maybe v, Map k v)
insertLookup key value m = let replaceStrategy _ _ old = old -- if the key is alread in the map, keep the old value
                           in M.insertLookupWithKey replaceStrategy key value m

-- Register a new operator.
-- In case the operator has already been registered, an error is issued
registerOperator :: Name -> OperatorInfo -> Parser ()
registerOperator name info = do
    (oldOp, newMap) <- insertLookup name info <$> get
    -- Check for duplicates
    case oldOp of
        -- TODO: probably it's better to use a custom error
        Just _  -> fail "Duplicate operator registration"
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

-- Parse a lexeme and ignore spaces and comments
lexeme :: Parser a -> Parser a
lexeme = L.lexeme ignoreSpaceAndComents

-- Parse a symbol and ignore spaces and comments
symbol :: Text -> Parser ()
symbol = void . L.symbol ignoreSpaceAndComents

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

text :: Parser Text
text = lexeme $ do
    _ <- P.char '"'
    str <- P.manyTill L.charLiteral (P.char '"')
    return $ T.pack str

identifier :: Parser Name
identifier = lexeme $ normalIdentifier <|> quotedIdentifier
    where normalIdentifier = Name <$> P.some P.letterChar
          quotedIdentifier = do
              _ <- P.char '\''
              Name <$> P.someTill P.printChar (P.char '\'')


-- Parser definition

-- Helper functions
betweenParens :: Parser a -> Parser a
betweenParens = P.between (symbol "(") (symbol ")")

-- Declarations

-- EBNF:
-- ArgList ::= '(' (identifier ',')* ')'
-- Decl    ::= identifier '=' Expr
--           | identifier ArgList '=' Expr

parseDecl :: Parser Decl
parseDecl = do
    name <- identifier
    TermBinding <$> parseTermBinding name

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

parseTermBinding :: Name -> Parser TermDecl
parseTermBinding name = do
    termBinding <- parseNameDecl name <|> parseFunDecl name
    return termBinding

-- Expressions

-- EBNF:
-- PrimaryExpr ::= LetExpr
--               | LambdaExpr
--               | identifier
--               | Literal
--               | '(' Expr ')'
--
-- ExprList ::= '(' (Expr ',')* ')'
--
-- Term ::= PrimaryExpr (ExprList)*
--
-- Expr ::= Expr identifier Expr
--        | Term

-- TODO: Implement infix operators
parseExpr :: Parser Expr
parseExpr = let noOp = OperatorInfo InfixN (-1)
            in parseExpr' noOp

-- Precedence Parser

-- Given two operators determine the precedence rules
data PrecRule = LeftBindsTighter
              | RightBindsTighter
              | CannotMix

comparePrec :: OperatorInfo -> OperatorInfo -> PrecRule
comparePrec left right = case compare (prec left) (prec right) of
    LT -> LeftBindsTighter
    GT -> RightBindsTighter
    EQ -> if (assoc left) /= (assoc right)
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
    where go :: Parser Expr
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
parsePrimary = P.choice [ parseLet
                        , parsePrimaryStartingWithIdent
                        , parsePrimaryStartingWithParen
                        , parseLiteral
                        ]

parseLet :: Parser Expr
parseLet = do
    symbol "let"
    (TermDecl name binding) <- identifier >>= parseTermBinding
    symbol ","
    expr <- parseExpr
    return $ Let name binding expr

parsePrimaryStartingWithIdent :: Parser Expr
parsePrimaryStartingWithIdent = do
    ident <- identifier
    P.choice [ parseSingleArgLambda ident
             , pure (Var ident)
             ]

parseSingleArgLambda :: Name -> Parser Expr
parseSingleArgLambda arg = do
    symbol "=>"
    expr <- parseExpr
    return $ Lam arg expr

-- TODO: Try removing this 'try'
parsePrimaryStartingWithParen :: Parser Expr
parsePrimaryStartingWithParen = parseMultiArgLambda <|> betweenParens parseExpr
    where parseMultiArgLambda = do
              -- Wrap the first part in a try, because there can be
              -- common prefixes between parseMultiArgLambda
              -- and a parenthesized expression, but once we find a =>
              -- we are sure that what's coming ahead must be a lambda expression
              args <- P.try $ parseArgList <* symbol "=>"
              expr <- parseExpr
              return $ variadicLambda args expr



parseLiteral :: Parser Expr
parseLiteral = P.choice [ Double <$> P.try float
                        , Int <$> integer
                        , Text <$> text
                        ]

-- Run parser and in case of error pretty print the error message
runParser :: Parser a -> Text -> Either String a
runParser p input = let parser = ignoreSpaceAndComents >> p
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
runParseReplStmt = let parser = P.choice [ Decl <$> P.try parseDecl
                                        , Expr <$> parseExpr
                                        ]
                   in runParser parser

