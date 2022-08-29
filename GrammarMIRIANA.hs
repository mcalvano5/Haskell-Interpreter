module GrammarMIRIANA where
 --  In Haskell, you can have many constructors for your data type, 
 --  separated by a vertical bar |. Each of your constructors then has its own list of data types

data Type = 
    IntType Int
    | FloatType Float
    | CharType Char
    | BoolType Bool
    | ArrayTypeInt [Int]
    deriving Show 

--Arithmetic expressions
data ArithExpr =
    Constant Int
    | Const Float
    | ArithVariable String
    | ArrayVariable String ArithExpr
    | Sum ArithExpr ArithExpr
    | Sub ArithExpr ArithExpr
    | Mul ArithExpr ArithExpr
    | Div ArithExpr ArithExpr
    | Power ArithExpr ArithExpr
    | Sqrt ArithExpr ArithExpr
    deriving Show

data ArrayExpr =                -- all expression that give Array as result
    Array [ArithExpr]
    | ArrVariable String
    deriving Show


-- Boolean expressions
data BoolExpr =
    Boolean Bool
    | BooleanIdentifier String
    | Or BoolExpr BoolExpr
    | Empty String
    | And BoolExpr BoolExpr
    | Not BoolExpr
    | LowerThan ArithExpr ArithExpr
    | GreaterThan ArithExpr ArithExpr
    | LowerEqualThan ArithExpr ArithExpr
    | GreaterEqualThan ArithExpr ArithExpr
    | EqualTo ArithExpr ArithExpr
    | Different ArithExpr ArithExpr
    deriving Show         

-- Commands
data Command =
    IfThenElse BoolExpr [Command] [Command]
    | While BoolExpr [Command]
    | ArithDeclare String ArithExpr
    | BoolDeclare String BoolExpr
    | ArrayDeclare String ArithExpr
    | ArithAssign String ArithExpr
    | BoolAssign String BoolExpr   
    | ArrOneAssign String ArithExpr ArithExpr --Assign a value to a specific array cell. Example: array[0] = 1
    | ArrMulAssign String ArithExpr --Assign whole array to a pre-declared variable. Example: a = [1,4,6]
    | Skip
    deriving Show   


