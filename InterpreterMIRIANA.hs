{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use <$>" #-}
module InterpreterMIRIANA where
import ArrayMIRIANA
import GrammarMIRIANA    

-- These are the components of the Environment

data Variable = Variable {name  :: String,
                          value :: Type } deriving Show

-- Array of tuples of type Variable
type Env = [Variable]

-- I define the operations to exploit the Env

-- This modifies the Environment after some modification to variables.

modifyEnv :: Env -> Variable -> Env 
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar = 
    if (name x) == (name newVar) 
        then [newVar] ++ xs 
    else[x] ++ modifyEnv xs newVar


-- This returns the value of the variable 

searchVariable:: Env -> String-> Maybe Type
searchVariable [] varname = Nothing
searchVariable (x:xs) varname = if (name x) == varname
        then Just (value x)
                                else searchVariable xs
                                varname


--ARITHMETIC EXPRESSION EVALUATION--
arithExprEval:: Env -> ArithExpr -> Maybe Int
arithExprEval env (Constant c) = Just c

arithExprEval env (ArithVariable c) = 
    case searchVariable env c of
        Just (IntType v)-> Just v
        Just (FloatType v)-> Just v
        Just (BoolType _) -> error "Variable of type boolean!"
        Just (ArrayType _) -> error "Variable of type array!"
        Nothing -> error "undeclared variable"

arithExprEval env (ArrVariable s c) =
    case searchVariable env s of 
        Just (ArrayType a)-> Just (readElemArray a j)
            where Just j = arithExprEval env c
        Just (IntType v)-> error "Variable of type integer!"
        Just (FloatType v)-> error "Variable of type float!"
        Just (BoolType _) -> error "Variable of type boolean!"
        Nothing -> error "undeclared variable"

arithExprEval env (Sum a b) =  pure (+) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Sub a b) = pure (-) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Mul a b) = pure (*) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Div a b) = pure (div) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Power a b) = pure (^) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Sqrt a b) = pure (!^) <*> (arithExprEval env a) <*> (arithExprEval env b)


-- BOOLEAN EXPRESSION EVALUATION

boolExprEval :: Env -> BoolExpr -> Maybe Bool

boolExprEval env (Boolean b) = Just b

boolExprEval env ( BooleanIdentifier id_bool)=
    case searchVariable env id_bool of 
        Just (BoolType v) -> Just v
        Just (IntType v)-> error "Variable of type integer!"
        Just (FloatType v)-> error "Variable of type float!"
        Just (ArrayType _) -> error "Variable of type array!"
        Nothing -> error "undeclared variable"

boolExprEval env (LowerThan a b) = pure (<) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (GreaterThan a b) = pure (>) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (EqualTo a b) = pure (==) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (Different a b) = pure (/=) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (LowerEqualThan a b) = pure (<=) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (GreaterEqualThan a b) = pure (>=) <*> (arithExprEval env a) <*> (arithExprEval env b)

boolExprEval env (And a b) = pure (&&) <*> (boolExprEval env a) <*> (boolExprEval env b)

boolExprEval env (Or a b) = pure (||) <*> (boolExprEval env a) <*> (boolExprEval env b)

boolExprEval env (Not a) = not <$> boolExprEval env a      

-- ARRAY EXPRESSION EVALUATION

arrExprEval :: Env -> ArrayExpr -> Maybe [Int]
arrExprEval e (Array a) = if hasFailed 
                                    then Nothing
                                    else Just $ map (\v -> case v of Just x -> x) r
                                      where hasFailed = or $ map (\v -> case v of
                                              Nothing -> True
                                              Just x -> False) r
                                            r = map (\exp -> arithExprEval e exp) a
arrayExpEval s (ArrVariable v) = 
  case get s v of
    Just (IntType _) -> error "Assignment of an integer value to an array one not allowed!"
    Just (FloatType v)-> error "Assignment of a float value to an array one not allowed!"
    Just (BoolType _) -> error "Assignment of an boolean value to an array one not allowed!"
    Just (ArrayType a) -> Just a
    Nothing -> error "Variable to assign not found"

-- EXECUTION OF THE PROGRAM--

execProgr :: Env -> [Command] -> Env

execProgr e [] = e 

execProgr e  (Skip : cs) = execProgr e cs

execProgr e ((IfThenElse b nc nc') : cs) =
        case boolExprEval e b of
                Just True -> execProgr e (nc ++ cs)
                Just False-> execProgr e (nc'++ cs)
                Nothing -> error "Error if"


execProgr e ((While b nc) : cs) =
        case boolExprEval e b of
                Just True -> execProgr e (nc ++ [(Whiledo b nc)] ++ cs)
                Just False -> execProgr e cs
                Nothing -> error "Error while"

execProgr e ((ArithAssign s a) : cs ) =
        case searchVariable e s of
                Just (IntType _ ) -> execProgr (modifyEnv e var) cs
                               where var = Variable s (IntType z)
                                        where Just z = arithExprEval e a
                Just _ -> error "Type mismatch"
                Nothing -> error "Error assign" 


execProgr e ((BoolAssign s b) : cs ) =
        case searchVariable e s of
                Just (BoolType _ ) -> execProgr (modifyEnv e var) cs
                               where var = Variable s (BoolType z)
                                        where Just z = boolExprEval e b
                Just _ -> error "Type mismatch"
                Nothing -> error "Error assign" 

execProgr e (( ArithDeclare s a ) : cs ) =
        case arithExprEval e a of
                Just exp -> case searchVariable e s of
                        Just _ -> error "double declaration"
                        Nothing -> execProgr (modifyEnv e var) cs
                               where var = Variable s (IntType z)
                                        where Just z = arithExprEval e a
                Nothing -> error "Error declare"

execProgr e (( BoolDeclare s a ) : cs ) =
        case boolExprEval e a of
                Just exp -> case searchVariable e s of
                        Just _ -> error "double declaration"
                        Nothing -> execProgr (modifyEnv e var) cs
                               where var = Variable s (BoolType z)
                                        where Just z = boolExprEval e a
                Nothing -> error "Error declare"


execProgr e ((ArrOneAssign s i a) : cs ) =
        case searchVariable e s of
                Just (ArrayType x ) -> execProgr (modifyEnv e var) cs
                               where var = Variable s (ArrayType z)
                                        where z = insertElemArray x j a' 
                                                where   
                                                        Just a'= arithExprEval e a 
                                                        Just j = arithExprEval e i
                Just _ -> error "Type mismatch"
                Nothing -> error "Error assign" 


execProgr e ((ArrayDeclare s a) : cs ) =
        case searchVariable e s of
                Just _ -> error "double declaration"
                Nothing -> execProgr (modifyEnv e var) cs
                         where var = Variable s (ArrayType z)
                                 where z = declareArray j 
                                        where Just j = arithExprEval e a
                Nothing -> error "Error declare"

execProgr env ((ArrMulAssign v exp) : cs) =
  case searchVariable env v of
    Just (ArrayType a) -> case arrExprEval env exp of
                            Just b -> if length a == length  b 
                            then
                                execProgr (modifyEnv env (Variable v (ArrayType b))) cs
                            else error "Length not valid!"
                            Nothing -> error "aExp evaluation of array failed"
    Nothing -> error "Undeclared variable!"
