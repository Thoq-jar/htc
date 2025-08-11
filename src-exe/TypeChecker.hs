module TypeChecker (checkTypes, stripTypes) where

import AST
import Parser

checkTypes :: String -> String
checkTypes input =
  case parseProgram input of
    Left err -> "Parse error: " ++ err
    Right stmts ->
      case typeCheck [] stmts of
        Left err -> "Type error: " ++ err
        Right _ -> "Type checking passed"

stripTypes :: String -> String
stripTypes input =
  case parseProgram input of
    Left err -> "Parse error: " ++ err
    Right stmts -> generateJS stmts

generateJS :: [Statement] -> String
generateJS stmts = unlines $ map statementToJS stmts

statementToJS :: Statement -> String
statementToJS (VarDecl name _ (Just expr)) =
  "let " ++ name ++ " = " ++ exprToJS expr ++ ";"
statementToJS (VarDecl name _ Nothing) =
  "let " ++ name ++ ";"
statementToJS (FuncDecl name params _ body) =
  let paramNames = map fst params
      paramList = case paramNames of
        [] -> ""
        [p] -> p
        ps -> foldr1 (\x acc -> x ++ ", " ++ acc) ps
   in "function "
        ++ name
        ++ "("
        ++ paramList
        ++ ") {\n"
        ++ concatMap (("  " ++) . (++ "\n") . statementToJS) body
        ++ "}"
statementToJS (ExprStmt expr) =
  exprToJS expr ++ ";"
statementToJS (ReturnStmt (Just expr)) =
  "return " ++ exprToJS expr ++ ";"
statementToJS (ReturnStmt Nothing) =
  "return;"

exprToJS :: Expr -> String
exprToJS (NumberLit n) = show n
exprToJS (StringLit s) = show s
exprToJS (BooleanLit b) = if b then "true" else "false"
exprToJS (Identifier name) = name
exprToJS (FunctionCall name args) =
  let argList = case args of
        [] -> ""
        [a] -> exprToJS a
        as -> foldr1 (\x acc -> x ++ ", " ++ acc) (map exprToJS as)
   in name ++ "(" ++ argList ++ ")"
exprToJS (MethodCall obj method args) =
  let argList = case args of
        [] -> ""
        [a] -> exprToJS a
        as -> foldr1 (\x acc -> x ++ ", " ++ acc) (map exprToJS as)
   in exprToJS obj ++ "." ++ method ++ "(" ++ argList ++ ")"
exprToJS (BinaryOp op left right) =
  exprToJS left ++ " " ++ op ++ " " ++ exprToJS right

typeCheck :: TypeEnv -> [Statement] -> Either String TypeEnv
typeCheck env [] = Right env
typeCheck env (stmt : stmts) =
  case typeCheckStatement env stmt of
    Left err -> Left err
    Right env' -> typeCheck env' stmts

typeCheckStatement :: TypeEnv -> Statement -> Either String TypeEnv
isKnownType :: TSType -> Bool
isKnownType NumberType = True
isKnownType StringType = True
isKnownType BooleanType = True
isKnownType VoidType = True
isKnownType (FunctionType params ret) = all isKnownType params && isKnownType ret
isKnownType UnknownType = False
typeCheckStatement env (VarDecl name Nothing (Just expr)) =
  case inferType env expr of
    Left err -> Left err
    Right exprType ->
      if isKnownType exprType
        then Right ((name, exprType) : env)
        else Left $ "Unknown type: " ++ show exprType ++ " in variable " ++ name
typeCheckStatement env (VarDecl name (Just declType) (Just expr)) =
  if not (isKnownType declType)
    then Left $ "Unknown declared type: " ++ show declType ++ " in variable " ++ name
    else case inferType env expr of
      Left err -> Left err
      Right exprType ->
        if declType == exprType
          then Right ((name, declType) : env)
          else Left $ "Type mismatch: expected " ++ show declType ++ ", got " ++ show exprType
typeCheckStatement env (VarDecl name (Just declType) Nothing) =
  if isKnownType declType
    then Right ((name, declType) : env)
    else Left $ "Unknown declared type: " ++ show declType ++ " in variable " ++ name
typeCheckStatement env (VarDecl name Nothing Nothing) =
  Left $ "Variable " ++ name ++ " declared without type or initializer"
typeCheckStatement env (FuncDecl name params retType body)
  | not (isKnownType retType) = Left $ "Unknown return type: " ++ show retType ++ " in function " ++ name
  | not (all (isKnownType . snd) params) = Left $ "Unknown parameter type in function " ++ name
  | otherwise =
      let funcType = FunctionType (map snd params) retType
          funcEnv = (name, funcType) : env
          paramEnv = params ++ funcEnv
       in case typeCheckStatements paramEnv body retType of
            Left err -> Left err
            Right _ -> Right ((name, funcType) : env)
typeCheckStatement env (ExprStmt expr) =
  case inferType env expr of
    Left err -> Left err
    Right exprType ->
      if isKnownType exprType
        then Right env
        else Left $ "Unknown type: " ++ show exprType ++ " in expression statement"
typeCheckStatement env (ReturnStmt (Just expr)) =
  case inferType env expr of
    Left err -> Left err
    Right exprType ->
      if isKnownType exprType
        then Right env
        else Left $ "Unknown type: " ++ show exprType ++ " in return statement"
typeCheckStatement env (ReturnStmt Nothing) = Right env

typeCheckStatements :: TypeEnv -> [Statement] -> TSType -> Either String ()
typeCheckStatements env stmts expectedReturn =
  let checkAll = checkAllStatements env stmts expectedReturn
   in case expectedReturn of
        VoidType -> checkAll
        _ ->
          case findReturn stmts of
            Nothing -> Left $ "Function is declared to return " ++ show expectedReturn ++ " but does not return a value"
            Just expr ->
              case inferType env expr of
                Left err -> Left err
                Right t ->
                  if t == expectedReturn
                    then checkAll
                    else Left $ "Function is declared to return " ++ show expectedReturn ++ " but returns " ++ show t

checkAllStatements :: TypeEnv -> [Statement] -> TSType -> Either String ()
checkAllStatements _ [] _ = Right ()
checkAllStatements env (stmt : stmts) expectedReturn =
  case typeCheckStatement env stmt of
    Left err -> Left err
    Right env' -> checkAllStatements env' stmts expectedReturn

findReturn :: [Statement] -> Maybe Expr
findReturn [] = Nothing
findReturn (ReturnStmt (Just expr) : _) = Just expr
findReturn (_ : rest) = findReturn rest

inferType :: TypeEnv -> Expr -> Either String TSType
inferType _ (NumberLit _) = Right NumberType
inferType _ (StringLit _) = Right StringType
inferType _ (BooleanLit _) = Right BooleanType
inferType env (Identifier name) =
  case lookup name env of
    Just t -> Right t
    Nothing -> Left $ "Undefined variable: " ++ name
inferType env (FunctionCall name args) =
  case lookup name env of
    Just (FunctionType paramTypes retType) ->
      if length args /= length paramTypes
        then Left $ "Wrong number of arguments to " ++ name
        else do
          argTypes <- mapM (inferType env) args
          if argTypes == paramTypes
            then Right retType
            else Left $ "Argument type mismatch in call to " ++ name
    Just _ -> Left $ name ++ " is not a function"
    Nothing -> Left $ "Undefined function: " ++ name
inferType env (MethodCall obj method args) =
  case method of
    "log" -> Right VoidType
    _ -> Left $ "Unknown method: " ++ method
inferType env (BinaryOp "+" left right) =
  case (inferType env left, inferType env right) of
    (Right NumberType, Right NumberType) -> Right NumberType
    (Right StringType, Right StringType) -> Right StringType
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    _ -> Left "Type mismatch in addition"
inferType env (BinaryOp op left right) =
  case (inferType env left, inferType env right) of
    (Right NumberType, Right NumberType) -> Right NumberType
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    _ -> Left $ "Type mismatch in " ++ op
