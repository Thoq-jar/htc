module AST where

data TSType
  = NumberType
  | StringType
  | BooleanType
  | VoidType
  | UnknownType
  | FunctionType [TSType] TSType
  deriving (Eq, Show)

data Expr
  = NumberLit Double
  | StringLit String
  | BooleanLit Bool
  | Identifier String
  | FunctionCall String [Expr]
  | MethodCall Expr String [Expr]
  | BinaryOp String Expr Expr
  deriving (Show)

data Statement
  = VarDecl String (Maybe TSType) (Maybe Expr)
  | FuncDecl String [(String, TSType)] TSType [Statement]
  | ExprStmt Expr
  | ReturnStmt (Maybe Expr)
  deriving (Show)

type TypeEnv = [(String, TSType)]
