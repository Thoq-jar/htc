module Parser (parseProgram) where

import AST
import Data.Char (isAlpha, isAlphaNum, isSpace)

parseProgram :: String -> Either String [Statement]
parseProgram input = parseStatements (tokenize input)

tokenize :: String -> [String]
tokenize [] = []
tokenize (c : cs)
  | isSpace c = tokenize cs
  | c == '(' = "(" : tokenize cs
  | c == ')' = ")" : tokenize cs
  | c == '{' = "{" : tokenize cs
  | c == '}' = "}" : tokenize cs
  | c == ';' = ";" : tokenize cs
  | c == ':' = ":" : tokenize cs
  | c == '=' = "=" : tokenize cs
  | c == '+' = "+" : tokenize cs
  | c == '-' = "-" : tokenize cs
  | c == '*' = "*" : tokenize cs
  | c == '/' = "/" : tokenize cs
  | c == '.' = "." : tokenize cs
  | c == ',' = "," : tokenize cs
  | c == '"' =
      let (str, rest) = parseString cs
       in ('"' : str ++ "\"") : tokenize rest
  | isAlpha c =
      let (word, rest) = span (\x -> isAlphaNum x || x == '_') (c : cs)
       in word : tokenize rest
  | otherwise =
      let (num, rest) = span (\x -> elem x "0123456789.") (c : cs)
       in if not (null num)
            then num : tokenize rest
            else [c] : tokenize cs

parseString :: String -> (String, String)
parseString [] = ("", "")
parseString ('"' : cs) = ("", cs)
parseString (c : cs) =
  let (str, rest) = parseString cs
   in (c : str, rest)

parseStatements :: [String] -> Either String [Statement]
parseStatements [] = Right []
parseStatements tokens =
  case parseStatement tokens of
    Left err -> Left err
    Right (stmt, rest) ->
      case parseStatements rest of
        Left err -> Left err
        Right stmts -> Right (stmt : stmts)

parseStatement :: [String] -> Either String (Statement, [String])
parseStatement ("let" : name : ":" : typeName : "=" : rest) =
  case parseType typeName of
    Left err -> Left err
    Right tsType ->
      case parseExpression rest of
        Left err -> Left err
        Right (expr, ";" : remaining) -> Right (VarDecl name (Just tsType) (Just expr), remaining)
        Right (_, _) -> Left "Expected ';'"
parseStatement ("let" : name : "=" : rest) =
  case parseExpression rest of
    Left err -> Left err
    Right (expr, ";" : remaining) -> Right (VarDecl name Nothing (Just expr), remaining)
    Right (_, _) -> Left "Expected ';'"
parseStatement ("function" : name : "(" : rest) =
  case parseParameters rest of
    Left err -> Left err
    Right (params, ")" : ":" : retType : "{" : bodyTokens) ->
      case parseType retType of
        Left err -> Left err
        Right tsRetType ->
          case parseBlock bodyTokens of
            Left err -> Left err
            Right (body, remaining) -> Right (FuncDecl name params tsRetType body, remaining)
    Right (_, _) -> Left "Invalid function syntax"
parseStatement ("return" : rest) =
  case rest of
    ";" : remaining -> Right (ReturnStmt Nothing, remaining)
    _ -> case parseExpression rest of
      Left err -> Left err
      Right (expr, ";" : remaining) -> Right (ReturnStmt (Just expr), remaining)
      Right (_, _) -> Left "Expected ';'"
parseStatement tokens =
  case parseExpression tokens of
    Left err -> Left err
    Right (expr, ";" : remaining) -> Right (ExprStmt expr, remaining)
    Right (_, _) -> Left "Expected ';'"

parseParameters :: [String] -> Either String ([(String, TSType)], [String])
parseParameters (")" : rest) = Right ([], ")" : rest)
parseParameters (name : ":" : typeName : rest) =
  case parseType typeName of
    Left err -> Left err
    Right tsType ->
      case rest of
        "," : remaining ->
          case parseParameters remaining of
            Left err -> Left err
            Right (params, tokens) -> Right ((name, tsType) : params, tokens)
        _ -> Right ([(name, tsType)], rest)
parseParameters _ = Left "Invalid parameter syntax"

parseBlock :: [String] -> Either String ([Statement], [String])
parseBlock tokens = parseBlockStatements tokens 0

parseBlockStatements :: [String] -> Int -> Either String ([Statement], [String])
parseBlockStatements [] _ = Left "Unexpected end of input"
parseBlockStatements ("}" : rest) 0 = Right ([], rest)
parseBlockStatements ("}" : rest) n = parseBlockStatements rest (n - 1)
parseBlockStatements ("{" : rest) n = parseBlockStatements rest (n + 1)
parseBlockStatements tokens n =
  case parseStatement tokens of
    Left err -> Left err
    Right (stmt, remaining) ->
      case parseBlockStatements remaining n of
        Left err -> Left err
        Right (stmts, tokens') -> Right (stmt : stmts, tokens')

parseExpression :: [String] -> Either String (Expr, [String])
parseExpression (name : "." : method : "(" : rest) =
  case parseArguments rest of
    Left err -> Left err
    Right (args, remaining) -> Right (MethodCall (Identifier name) method args, remaining)
parseExpression (name : "(" : rest) =
  case parseArguments rest of
    Left err -> Left err
    Right (args, remaining) -> Right (FunctionCall name args, remaining)
parseExpression (name : "+" : rest) =
  case parseExpression rest of
    Left err -> Left err
    Right (expr, remaining) -> Right (BinaryOp "+" (Identifier name) expr, remaining)
parseExpression (name : rest)
  | all (\c -> elem c "0123456789.") name = Right (NumberLit (read name), rest)
  | not (null name) && head name == '"' && last name == '"' = Right (StringLit (init (tail name)), rest)
  | name == "true" = Right (BooleanLit True, rest)
  | name == "false" = Right (BooleanLit False, rest)
  | otherwise = Right (Identifier name, rest)
parseExpression [] = Left "Expected expression"

parseArguments :: [String] -> Either String ([Expr], [String])
parseArguments (")" : rest) = Right ([], rest)
parseArguments tokens =
  case parseExpression tokens of
    Left err -> Left err
    Right (expr, "," : remaining) ->
      case parseArguments remaining of
        Left err -> Left err
        Right (exprs, tokens') -> Right (expr : exprs, tokens')
    Right (expr, ")" : remaining) -> Right ([expr], remaining)
    Right (_, _) -> Left "Expected ')' or ','"

parseType :: String -> Either String TSType
parseType "number" = Right NumberType
parseType "string" = Right StringType
parseType "boolean" = Right BooleanType
parseType "void" = Right VoidType
parseType _ = Right UnknownType
