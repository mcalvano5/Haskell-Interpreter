{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module InterpreterMIRIANA where
import ArrayMIRIANA
import GrammarMIRIANA 
import Dictionary   

type Env = Dict String Type

emptyState :: Env
emptyState = empty


--ARITHMETIC EXPRESSION EVALUATION--
arithExprEval:: Env -> ArithExpr -> Maybe Int

arithExprEval env (Constant c) = Just c

arithExprEval env (ArithVariable c) = 
    case get env c of
        Just (IntType v)-> Just v
        Just (BoolType _) -> error "Variable of type boolean!"
        Just (ArrayType _) -> error "Variable of type array!"
        Nothing -> error "undeclared variable"


arithExprEval env (ArrayVariable s c) =
    case get env s of 
        Just (ArrayType a)-> Just (readElemArray a j)
            where Just j = arithExprEval env c
        Just (IntType _)-> error "Variable of type integer!"
        Just (BoolType _) -> error "Variable of type boolean!"
        Nothing -> error "undeclared variable"

arithExprEval env (Sum a b) =  pure (+) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Sub a b) = pure (-) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Mul a b) = pure (*) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Div a b) = pure (div) <*> (arithExprEval env a) <*> (arithExprEval env b)

arithExprEval env (Power a b) = pure (^) <*> (arithExprEval env a) <*> (arithExprEval env b)



arithExprEvalFloat:: Env -> ArithExpr -> Maybe Float
arithExprEvalFloat env (ArithVariable c) = 
  case get env c of
        Just (FloatType v)-> Just v
        Just (BoolType _) -> error "Variable of type boolean!"
        Just (ArrayType _) -> error "Variable of type array!"
        Nothing -> error "undeclared variable"--

-- BOOLEAN EXPRESSION EVALUATION

boolExprEval :: Env -> BoolExpr -> Maybe Bool

boolExprEval env (Boolean b) = Just b

boolExprEval env ( BooleanIdentifier id_bool)=
    case get env id_bool of 
        Just (BoolType v) -> Just v
        Just (IntType _)-> error "Variable of type integer!"
        Just (FloatType _)-> error "Variable of type float!"
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
arrExprEval e (ArrVariable v) = 
  case get e v of
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
                Just True -> execProgr e (nc ++ [(While b nc)] ++ cs)
                Just False -> execProgr e cs
                Nothing -> error "Error while"

execProgr e ((ArithAssign s ex) : cs ) =
        case get e s of
                Just (IntType _) -> execProgr (insert e s (IntType ex')) cs
                        where
                                Just ex' = arithExprEval e ex
                Just (FloatType _)-> error "Assignment of a float value to an array one not allowed!"
                Just (BoolType _) -> error "Assignment of a boolean value to an array one not allowed!"
                Just (ArrayType _) -> error "Assignment of an array value to an array one not allowed!"
                Nothing -> error "Error assign" 


execProgr e ((BoolAssign s ex) : cs ) =
        case get e s of
                Just (BoolType _) -> execProgr (insert e s (BoolType ex')) cs
                        where
                                Just ex' = boolExprEval e ex
                Just (IntType _)-> error "Assignment of an integer value to an array one not allowed!"
                Just (FloatType _)-> error "Assignment of a float value to an array one not allowed!"
                Just (ArrayType _)-> error "Assignment of an array value to an array one not allowed!"
                Nothing -> error "Error assign" 

execProgr e (( ArithDeclare s ex ) : cs ) =
        case arithExprEval e ex of
                Just ex' -> case get e s of
                        Just _ -> error "MultipleDeclaration"
                        Nothing -> execProgr (insert e s (IntType ex')) cs
                Nothing -> error "InvalidArithmeticExpression"


execProgr e (( BoolDeclare s ex ) : cs ) =
     case boolExprEval e ex of
        Just ex' -> case get e s of
                Just _ -> error "MultipleDeclaration"
                Nothing -> execProgr (insert e s (BoolType ex')) cs
        Nothing -> error "InvalidBooleanExpression"


execProgr e ((ArrOneAssign s i ex) : cs ) =
        case get e s of
                Just (ArrayType a ) -> execProgr (insert e s (ArrayType (insertElemArray a j ex'))) cs
                        where 
                                Just ex'= arithExprEval e ex 
                                Just j = arithExprEval e i
                Just (BoolType _)-> error "Assignment of a bool value to an array one not allowed!"
                Just (FloatType _)-> error "Assignment of a float value to an array one not allowed!"
                Just (IntType _)-> error "Assignment of an integer value to an array one not allowed!"
                Nothing -> error "Error assign" 

execProgr e ((ArrayDeclare s i) : cs ) =
        case get e s of
                Just _ -> error "double declaration"
                Nothing -> execProgr (insert e s (ArrayType (arrayDeclaration j))) cs
                        where Just j = arithExprEval e i


--execProgr e ((ArrMulAssign v exp) : cs) =
  --case get e v of
    --Just (ArrayType a) -> case arrExprEval e exp of
      --                      Just b -> if length a == length  b 
        --                    then
          --                      execProgr (insert e s (Variable v (ArrayType b))) cs
             --               else error "Length not valid!"
               --             Nothing -> error "arithExp evaluation of array failed"
    --Nothing -> error "Undeclared variable!"