{-# OPTIONS_GHC -Wno-missing-export-lists #-}
--This module contains the skeleton code for the assignment.
--
-- Please do not change the names of the parseExerciseX functions, as they
-- are used by the test suite.
--
-- You may, and are highly encouraged, to create your functions.
module Assignment where

import Instances
import Parser
import Control.Applicative
import Data.List ( isInfixOf )




data ADTValue
  = ADTInt Int
  | ADTString String
  | ADTArray [ADTValue]
  | ADTTrue
  | ADTFalse
  deriving (Show, Eq)

data Expression = And Expression Expression
  | Or Expression Expression
  | Not Expression
  | TrueExpr
  | FalseExpr
  | Plus Expression Expression
  | Minus Expression Expression
  | Times Expression Expression
  | Divide Expression Expression
  | Power Expression Expression
  | Equal Expression Expression
  | NotEqual Expression Expression
  | GreaterThan Expression Expression
  | LessThan Expression Expression
  | Ternary Expression Expression Expression
  | Value ADTValue
  | Variable String
  | Parameter [Expression]
  | FunctionCall Statement
  | FunctionCallBlock Expression Statement
  | EmptyExpression
  | Ret Statement
  deriving (Show, Eq)

data Statement = Assignment String Expression
  | Return Expression
  | VariableAssignment String Expression --variable function call
  | If Expression Statement
  | IfElse Statement Statement
  | Block [Statement]
  | IfThenElse Expression Expression Expression
  deriving (Show, Eq)

data ADT = Empty
  | Expression Expression
  | Statements [Statement]
  deriving (Eq, Show)

-- | Exercise A
--parsing values
parseExerciseA :: Parser ADT
parseExerciseA = Expression <$> parsePartA

--parsing Integer
parseInteger :: Parser Int
parseInteger =  spaces *> int <* spaces

--parsing True
parseTrue :: Parser Bool
parseTrue = stringTok "true" *> pure True

--parsing False
parseFalse :: Parser Bool
parseFalse = stringTok "false" *> pure False

--parsing String
parseString :: Parser String
parseString = spaces *> charTok '\"' *> many (noneof "\"" ) <* charTok '\"'

--wrap with ADTTrue
adtTrue :: Parser ADTValue
adtTrue = parseTrue *> pure ADTTrue

--wrap with ADTFalse
adtFalse :: Parser ADTValue
adtFalse = parseFalse *> pure ADTFalse

--wrap with ADTInt
adtInt :: Parser ADTValue
adtInt = ADTInt <$> parseInteger

--wrap with ADTString
adtString :: Parser ADTValue
adtString = ADTString <$> parseString

--wrap with adtBool
adtBool :: Parser ADTValue
adtBool =  adtTrue <|> adtFalse

-- Parser for Lists
parseList :: Parser ADTValue
parseList = is '[' *> (ADTArray <$> parseValue `sepBy` commaTok) <* is ']'

-- Parser for Values
parseValue :: Parser ADTValue
parseValue = asum [adtString, adtBool, adtInt, parseList, adtTrue, adtFalse]

--Exercise 2 logical expression 
--parse true 
logitrue :: Parser Expression
logitrue = parseTrue *> pure TrueExpr

--parse false
logiFalse :: Parser Expression
logiFalse = parseFalse *> pure FalseExpr

--parse true and false
logiBool :: Parser Expression
logiBool = logitrue <|> logiFalse

--parse and op
parseAnd :: Parser (Expression -> Expression -> Expression)
parseAnd = stringTok "&&" *> pure And

--parse Or op
parseOr :: Parser (Expression -> Expression -> Expression)
parseOr = stringTok "||" *> pure Or

--parse Not op
parseNot :: Parser (Expression -> Expression)
parseNot = stringTok "!" *> pure Not

--parse logical expression
parseLogi :: Parser (Expression -> Expression -> Expression)
parseLogi = parseAnd <|> parseOr

--Arithmetic Operation
-- Parse a binary op given as a character, ignoring spaces before and after the character
op :: Char -> Parser Char
op c = spaces >>= \i -> is c <* spaces

-- | Parse the "+" op
add :: Parser (Expression-> Expression-> Expression)
add = op '+' *> pure Plus

-- | Parse the "-" op
minus :: Parser (Expression-> Expression-> Expression)
minus = op '-' *> pure Minus

-- | Parse the "*" op
times :: Parser (Expression -> Expression -> Expression)
times = op '*' *> pure Times

-- | Parse the "**" op
power :: Parser (Expression -> Expression -> Expression)
power = stringTok "**" *> pure Power

-- | Parse the "/" op
divide :: Parser (Expression -> Expression -> Expression)
divide = op '/' *> pure Divide

--Parse Arithmetic operation
parseArith :: Parser (Expression->Expression->Expression)
parseArith = asum [add, minus, power, times, divide]

--Comparison
--parse Greater than
parseGt :: Parser (Expression-> Expression-> Expression)
parseGt = stringTok ">" *> pure GreaterThan

--parse Equal
parseEqualExpr :: Parser (Expression-> Expression-> Expression)
parseEqualExpr = stringTok "===" *> pure Equal

--parse Not Equal
parseNotEqualExpr :: Parser (Expression->Expression->Expression)
parseNotEqualExpr = stringTok "!==" *> pure NotEqual

--parse Less than
parseLessThanExpr :: Parser (Expression->Expression->Expression)
parseLessThanExpr = stringTok "<" *> pure LessThan

--parse comparison
parseComp :: Parser (Expression->Expression->Expression)
parseComp = asum [parseGt, parseEqualExpr, parseNotEqualExpr, parseLessThanExpr]

--parse expression that is in bracket 
bracketExpr :: Parser Expression
bracketExpr =
  do
    _ <- spaces
    _ <- is '('
    _<-spaces
    expre1 <-  parseFunctionCall <|> parsePartA --parse function call or parse expression eg true, a()
    _<- spaces
    expre2 <- many $ parseLogi <|> parseArith <|> parseComp >>=  --parse logical, arithmetic, comparison eg &&,||
                       \opr -> spaces *> parseFunctionCall <|> parsePartA >>=  --parses function call or expression eg true, a()
                       \expr -> pure (opr, expr) --chain the operator and expression in expre 2            
    _<-spaces
    _ <- is ')'
    _<- spaces
    pure $ foldl (\acc (opr, expr) -> opr acc expr) expre1 expre2
    --use foldl to apply operators in expre2 to expre1 

--parse not in bracket -> seperate cuz it has 1 argument
bracketNot :: Parser Expression
bracketNot =
  do
    _ <- spaces
    _ <- is '('
    _<-spaces
    operator <- parseNot
    _<- spaces
    expre <- bracketExpr <|> bracketNot <|> parseVal
    _<- spaces
    _ <- is ')'
    _<-spaces
    pure (operator expre)

parseVal :: Parser Expression
parseVal = Value <$> parseValue

--parse value inside the bracket
bracketValue :: Parser Expression
bracketValue = spaces >> is '(' >> parseValue >>= \expre -> is ')' >> pure (Value expre)

--parse ternary op
ternary :: Parser Expression
ternary = do
  _<-spaces
  _<-is '('
  _<-spaces
  expre1 <- parsePartA <|> ternary --can have nested ternary 
  _<-spaces
  operator <- stringTok "?"
  _<-spaces
  expre2 <- parsePartA <|> ternary
  _<-spaces
  op2 <- stringTok ":"
  _<-spaces
  expre3 <- parsePartA <|> ternary
  _<-spaces
  _<-is ')'
  pure (Ternary expre1 expre2 expre3)

--parse variable
parseVar :: Parser Expression
parseVar = Variable <$> parseVariable

--combine all the parser  
parsePartA :: Parser Expression
parsePartA = asum [bracketExpr, bracketNot, parseVal, bracketValue, ternary,parseVar]

--Pretty print an ADT value
prettyPrintValue :: ADTValue -> String
prettyPrintValue (ADTInt n) = show n
prettyPrintValue (ADTString x) = "\"" ++ x ++ "\""
prettyPrintValue ADTTrue = "true"
prettyPrintValue ADTFalse = "false"
prettyPrintValue (ADTArray values) = "[" ++ prettyPrintArray values ++ "]"

--Pretty print an array of ADT values
prettyPrintArray :: [ADTValue] -> String
prettyPrintArray [] = ""
prettyPrintArray [x] = prettyPrintValue x
prettyPrintArray (x:xs) = prettyPrintValue x ++ ", " ++ prettyPrintArray xs

--pattern match all the expression and pretty print them
prettyPrintExpression :: Expression -> String
prettyPrintExpression (And exp1 exp2) = "(" ++ prettyPrintExpression exp1 ++ " && " ++ prettyPrintExpression exp2 ++ ")"
prettyPrintExpression (Or exp1 exp2) = "(" ++ prettyPrintExpression exp1 ++ " || " ++ prettyPrintExpression exp2 ++ ")"
prettyPrintExpression (Not exp1) = "(!" ++ prettyPrintExpression exp1 ++ ")"
prettyPrintExpression TrueExpr = "true"
prettyPrintExpression FalseExpr = "false"
prettyPrintExpression (Plus exp1 exp2) = "(" ++ prettyPrintExpression exp1 ++ " + " ++ prettyPrintExpression exp2 ++ ")"
prettyPrintExpression (Minus exp1 exp2) = "(" ++ prettyPrintExpression exp1 ++ " - " ++ prettyPrintExpression exp2 ++ ")"
prettyPrintExpression (Power exp1 exp2) = "(" ++ prettyPrintExpression exp1 ++ " ** " ++ prettyPrintExpression exp2 ++ ")"
prettyPrintExpression (Times exp1 exp2) = "(" ++ prettyPrintExpression exp1 ++ " * " ++ prettyPrintExpression exp2 ++ ")"
prettyPrintExpression (Divide exp1 exp2) = "(" ++ prettyPrintExpression exp1 ++ " / " ++ prettyPrintExpression exp2 ++ ")"
prettyPrintExpression (Equal exp1 exp2) = "(" ++ prettyPrintExpression exp1 ++ " === " ++ prettyPrintExpression exp2 ++ ")"
prettyPrintExpression (NotEqual exp1 exp2) = "(" ++ prettyPrintExpression exp1 ++ " !== " ++ prettyPrintExpression exp2 ++ ")"
prettyPrintExpression (GreaterThan exp1 exp2) = "(" ++ prettyPrintExpression exp1 ++ " > " ++ prettyPrintExpression exp2 ++ ")"
prettyPrintExpression (LessThan exp1 exp2) = "(" ++ prettyPrintExpression exp1 ++ " < " ++ prettyPrintExpression exp2 ++ ")"
prettyPrintExpression (Ternary exp1 exp2 exp3) = splitTernary (Ternary exp1 exp2 exp3)
prettyPrintExpression (Value value) = prettyPrintValue value
prettyPrintExpression (Variable s) =  s
prettyPrintExpression (Parameter []) = ""
prettyPrintExpression (Parameter [x]) = prettyPrintExpression x

prettyPrintExpression (Parameter (x:xs)) = prettyPrintExpression x ++ ", " ++ 
  prettyPrintExpression (Parameter xs)
prettyPrintExpression (FunctionCall (Assignment name value)) = 
  prettyPrintStatementHead (Assignment name value) 0 True

prettyPrintExpression (FunctionCall (VariableAssignment name value)) = 
  prettyPrintStatementHead (VariableAssignment name value) 0 True

prettyPrintExpression (FunctionCallBlock expre (Block stmts)) = prettyPrintExpression expre ++ " " ++ 
  prettyPrintStatementHead (Block stmts) 0 True

prettyPrintExpression (FunctionCallBlock expre _) = prettyPrintExpression expre
prettyPrintExpression EmptyExpression = ""
prettyPrintExpression (Ret state) = prettyPrintStatementHead state 0 False 
prettyPrintExpression _ = ""


--pretty print ternary
prettyPrintTernary :: Expression -> String
prettyPrintTernary (Ternary exp1 exp2 exp3) = "(" ++ 
  prettyPrintExpression exp1 ++ " ? " ++ 
  prettyPrintExpression exp2 ++ " : " ++ 
  prettyPrintExpression exp3 ++ ")"
prettyPrintTernary _ =[]

--contruct split ternary when > 42
contSplitTernary :: Expression -> String
contSplitTernary (Ternary exp1 exp2 exp3) = "(" ++ 
  prettyPrintExpression exp1 ++ "\n? " ++ 
  prettyPrintExpression exp2 ++ "\n: " ++ 
  prettyPrintExpression exp3 ++ ")"
contSplitTernary _ = []

--split ternary decider -> when > 42
splitTernary :: Expression -> String
splitTernary (Ternary exp1 exp2 exp3) = if length (prettyPrintTernary (Ternary exp1 exp2 exp3)) > 42
  then contSplitTernary (Ternary exp1 exp2 exp3)
  else prettyPrintTernary (Ternary exp1 exp2 exp3)
splitTernary _ = []

--split string when > 42 and decide where to split 
splitString :: Int -> Int -> String -> String
splitString index origIndex str
    | length str <= index = str
    | index == 0 = str
    | compareChar (searchChar index str) " +/" = take (index + 1) str ++ 
      "\n" ++ splitDecider origIndex (drop (index + 1) str)
    | otherwise = splitString (index - 1) origIndex str

--split decider if >42 then split else return 
splitDecider :: Int -> String -> String
splitDecider n s = if checkSplit n s
  then splitString n n s
  else s

--check if there is a break in the line
checkBreakInLine :: Int -> String -> Bool
checkBreakInLine n s = isInfixOf "\n" (take n s)

--check string > 42 and there is no break in the line
checkSplit :: Int -> String -> Bool
checkSplit n s = (length s > 42) && not (checkBreakInLine (n + 2) s)

--search for char according to index
searchChar :: Int -> String -> Char
searchChar index str = str !! index

--check the char is in the str or not
compareChar :: Char -> String -> Bool
compareChar c str = elem c str

--pretty print expression
prettyPrintExerciseA :: ADT -> String
prettyPrintExerciseA (Expression e) = case prettyPrintExpression e of
  [] -> ""
  x -> splitDecider 42 x
prettyPrintExerciseA Empty = "You must write a pretty printer!"
prettyPrintExerciseA _ = "You must write a pretty printer!"

-- | Exercise B
--parse all the variable listed
parseVariable :: Parser String
parseVariable = do
  _ <- spaces
  variable <- many (oneof "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")
  _ <- spaces
  pure variable

--parse wrap parse variable in adtstring
parseMultiVariable :: Parser Expression
parseMultiVariable = do
  _ <- spaces
  variable <- parseVariable
  _ <- spaces
  pure (Value (ADTString variable))

--parse const variable
parseConst :: Parser Statement
parseConst = do
  _ <- spaces
  _ <- stringTok "const"
  _ <- spaces
  name <- parseVariable
  _ <- spaces
  _ <- is '='
  _ <- spaces
  value <- parsePartA <|> parseMultiVariable
  _ <- spaces
  _ <- is ';'
  _ <- spaces
  pure (Assignment name value)

--parse Block 
parseBlock :: Parser Statement
parseBlock = do
  _ <- spaces
  _ <- is '{'
  _ <- spaces
  statements <- parsePartB
  _ <- spaces
  _ <- is '}'
  _ <- spaces
  pure (Block statements)

--parse If statement
parseIf :: Parser Statement
parseIf = do
  _ <- spaces
  _ <- stringTok "if"
  _ <- spaces
  _ <- is '('
  _ <- spaces
  condition <- parsePartA <|> parseMultiVariable
  _ <- spaces
  _ <- is ')'
  _ <- spaces
  statement1 <- parseBlock --use back the block for if statement 
  _ <- spaces
  pure (If condition statement1)

--parse ifelse statement 
parseIfElse :: Parser Statement
parseIfElse = do
  _<-spaces
  ifstatement <- parseIf --use back the if parser
  _<-spaces
  _<-stringTok "else"
  _<-spaces
  expreBlock <- parseBlock --use back the block parser
  _<-spaces
  pure (IfElse ifstatement expreBlock)

--combine all the statement parser
parseStatements :: Parser Statement
parseStatements = asum [parseReturn, parseConst, parseBlock, parseIfElse, parseIf]

--many it -> parser nested statement
parsePartB :: Parser [Statement]
parsePartB = many parseStatements

parseExerciseB :: Parser ADT
parseExerciseB = Statements <$> parsePartB

--pretty print statement
prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB (Statements e) = if isMultilineStatement e  --if is a multiline then 
  then prettyPrintStatementMultiCheck e 0 True  --true -> pretty print with \n
  else prettyPrintStatementMultiCheck e 0 False --false -> pretty print without \n 
prettyPrintExerciseB _ = ""

--replicate the tabs according to tabCount
tabs :: Int -> [Char]
tabs tabCount = replicate tabCount '\t'

--Decide how to print the statement, multiline -> true, no multiline -> false
prettyPrintStatementMultiCheck :: [Statement] -> Int -> Bool -> String
prettyPrintStatementMultiCheck [] _ b= ""
prettyPrintStatementMultiCheck [x] tabCount b = tabs tabCount ++ prettyPrintStatementHead x tabCount b
prettyPrintStatementMultiCheck (x : xs) tabCount False = tabs tabCount ++
  prettyPrintStatementHead x tabCount False ++ " " ++                         --if not multi-> space
  prettyPrintStatementMultiCheck xs tabCount (isMultilineStatement xs)

prettyPrintStatementMultiCheck (x:xs) tabCount True = tabs tabCount ++
  prettyPrintStatementHead x tabCount (isMultilineStatement [x]) ++
  "\n" ++ prettyPrintStatementMultiCheck xs tabCount (isMultilineStatement xs) --if multiline -> print with \n
--another check at true -> edge case eg. under a block, the block is true, 
--then there is a if statement -> check again the if statement block
--eg { if(true) {havent check} }

--pretty print statement head of list
prettyPrintStatementHead :: Statement -> Int -> Bool -> String
prettyPrintStatementHead (Assignment name value) x b =   "const " ++
  name ++ " = " ++ prettyPrintExpression value ++ ";"

prettyPrintStatementHead (If condition statement) x b=  "if ( " ++
  prettyPrintExpression condition ++ " ) " ++ prettyPrintStatementHead statement x b

prettyPrintStatementHead (IfElse statement1 statement2) x b = prettyPrintStatementHead statement1 x b ++
   " else " ++ prettyPrintStatementHead statement2 x b

prettyPrintStatementHead (Block statements) x True = "{\n" ++ prettyPrintStatementMultiCheck statements (x + 1) True ++
   "\n\n" ++ replicate x '\t' ++ "}"          --if multiline -> print with multiline format 

prettyPrintStatementHead (Block statements) x False = "{ " ++ prettyPrintStatementMultiCheck statements 0 False ++ " }" --print this if no
prettyPrintStatementHead (Return value) x b = "return " ++ prettyPrintExpression value ++ ";"
prettyPrintStatementHead (VariableAssignment name value) x b=  name ++ "(" ++ prettyPrintExpression value ++ ")"
prettyPrintStatementHead _ _ _ = ""


--check if the statement is multiline
isMultilineStatement :: [Statement] -> Bool
isMultilineStatement stmts = (foldl addNum 0 stmts > 1) || any stmtsLength stmts --if >42 or >1 statement -> multiline
  where
    addNum acc stmt = acc + calcStmtNum stmt --add the number of statement in block

--calculate the number of statement in block
calcStmtNum :: Statement -> Int
calcStmtNum (Assignment _ _) = 1
calcStmtNum (If _ b) = calcStmtNum b
calcStmtNum (IfElse (If _ b1) b2) = calcStmtNum b1 + calcStmtNum b2
calcStmtNum (Block stmts) = sum (calcStmtNum <$> stmts) --sum all the statements in block
calcStmtNum (Return _) = 1
calcStmtNum _ = 0

--statement length > 42 
stmtsLength :: Statement -> Bool
stmtsLength (If expre stmt) = length (prettyPrintStatementHead (If expre stmt) 0 False) > 42
stmtsLength (IfElse stmt1 stmt2) = length (prettyPrintStatementHead (IfElse stmt1 stmt2) 0 False) > 42
stmtsLength (Block stmts) = length (prettyPrintStatementHead (Block stmts) 0 False) > 42
stmtsLength (Return expre) = length (prettyPrintStatementHead (Return expre) 0 False) > 42
stmtsLength _ = False


-- | Exercise C
-- This function should determine if the given code is a tail recursive function

--parse parameter
parseFunctionPara :: Parser Expression
parseFunctionPara = do
  _<-spaces
  _<- is '('
  _<-spaces
  variables <- sepBy1 parsePartA commaTok --parse all the variable separate by comma
  _<-spaces
  _<- is ')'
  pure (Parameter variables)

--parse function const eg const a = a(1,2,3)
parseFunctionConst :: Parser Expression
parseFunctionConst = do
  _<-spaces
  _<-stringTok "const"
  _<-spaces
  name <- parseVariable
  _<-spaces
  _<-is '='
  _<-spaces
  expre <-parseFunctionVar --use back function var parser eg a(1,2,3)
  _<-spaces
  pure (FunctionCall (Assignment name expre))

--parse function variable eg a(1,2,3)
parseFunctionVar :: Parser Expression
parseFunctionVar = do
  _ <- spaces
  name <- parseVariable
  _<- spaces
  expre <- parseFunctionPara
  _ <- spaces
  _ <- optional (is ';') --optional as sometimes there is no semicolon eg return (a(1,2,3)); 
  pure (FunctionCall (VariableAssignment name expre))

--parse all the function call, const b = a(1,2,3) and a(1,2,3)
parseFunctionCall :: Parser Expression
parseFunctionCall = parseFunctionVar <|> parseFunctionConst

--parse function block eg function a(1,2,3) {}
parseFunctionCallBlock :: Parser Expression
parseFunctionCallBlock = do
  _<-spaces
  _<-stringTok "function"
  _<-spaces
  expre <- parseFunctionVar <|> parsePartA
  _<-spaces
  state <- parseBlock --use back block parser
  _<-spaces
  pure (FunctionCallBlock expre state)

--parse return statement
parseReturn :: Parser Statement
parseReturn = do
  _<-spaces
  _<-stringTok "return"
  _<-spaces
  expre <- parseFunctionVar <|> parsePartA --used to parse eg return a(1,2,3) or return (1+2)
  _<-spaces
  _<- optional (is ';')
  _<-spaces
  pure (Return expre)

--parse all the function statement
parsePartC :: Parser Expression
parsePartC = asum [parseFunctionCallBlock, parseFunctionCall]

--critiria 1 function keyword, followed by zero or more parameters.
critiria1 :: Expression -> Bool
critiria1 (FunctionCallBlock _ _) = True
critiria1 _ = False

-- function calls for 2 expre
checkFunction :: Expression -> Expression -> Bool
checkFunction exp1 exp2 = checkFunctionExpre exp1 || checkFunctionExpre exp2

-- One or more return statements that do not involve the function name, nor other
-- function calls (i.e. only expressions involving const variables or literals)
checkFunctionExpre :: Expression -> Bool
checkFunctionExpre (FunctionCall stmt) = False --if there are function call -> False
checkFunctionExpre (FunctionCallBlock _ _) = False
checkFunctionExpre (Parameter x) = all checkFunctionExpre x
checkFunctionExpre (Plus exp1 exp2) = checkFunction exp1 exp2
checkFunctionExpre (Minus exp1 exp2) = checkFunction exp1 exp2
checkFunctionExpre (Times exp1 exp2) = checkFunction exp1 exp2
checkFunctionExpre (Divide exp1 exp2) = checkFunction exp1 exp2
checkFunctionExpre (Power exp1 exp2) = checkFunction exp1 exp2
checkFunctionExpre (Equal exp1 exp2) = checkFunction exp1 exp2
checkFunctionExpre (NotEqual exp1 exp2) = checkFunction exp1 exp2
checkFunctionExpre (GreaterThan exp1 exp2) = checkFunction exp1 exp2
checkFunctionExpre (LessThan exp1 exp2) = checkFunction exp1 exp2
checkFunctionExpre (Ternary exp1 exp2 exp3) = checkFunction exp1 exp2 || checkFunctionExpre exp3
checkFunctionExpre (Value _) = True                                 --if dh function call -> True
checkFunctionExpre (Variable _) = True
checkFunctionExpre (Not exp1) = checkFunctionExpre exp1
checkFunctionExpre _ = True

-- Check if a statement involves function calls
checkFunctionStatement :: Statement -> Bool
checkFunctionStatement (Assignment _ expr) = checkFunctionExpre expr
checkFunctionStatement (VariableAssignment _ expr) = checkFunctionExpre expr
checkFunctionStatement (If expr _) = checkFunctionExpre expr
checkFunctionStatement (IfElse state1 state2) = checkFunctionStatement state1 || checkFunctionStatement state2
checkFunctionStatement (Return expr) = checkFunctionExpre expr
checkFunctionStatement (Block stmt) = any checkFunctionStatement stmt
checkFunctionStatement _ = False

-- Check if return statement involves only the function name
checkFunctionName :: String -> Expression -> Int
checkFunctionName name (FunctionCall (VariableAssignment var _)) = if name == var then 1 else 0
checkFunctionName _ _ = 0    --if name same-> 1 else 0

-- Get all the return expressions in a block
getReturnExpr :: Statement -> [Expression]
getReturnExpr (Return expr) = [expr]
getReturnExpr (Block stmts) = concatMap getReturnExpr stmts --flatten the list using concatmap
getReturnExpr (If _ block) = getReturnExpr block
getReturnExpr (IfElse block1 block2) = getReturnExpr block1 ++ getReturnExpr block2
getReturnExpr _ = []

-- Check if there is exactly one return statement involving the function name
-- sum the same function name in the return statement
checkNumReturn :: String -> Statement -> Int
checkNumReturn name (Block [stmts])  = sum $ map (checkFunctionName name) (concatMap getReturnExpr [stmts])
--if only 1 statement -> sum the same function name in the return statement
--cover all cases
checkNumReturn name (Block stmts) = sum $ map (checkFunctionName name) (concatMap getReturnExpr stmts)
checkNumReturn name _ = 0

-- Check if there is exactly one return statement involving the function name
-- if more than one -> false
checkOneReturn :: String -> Statement-> Bool
checkOneReturn name (Block [stmts]) = checkNumReturn name (Block [stmts]) == 1
checkOneReturn name (Block stmts) = checkNumReturn name (Block stmts) == 1
checkOneReturn name _ = False

--check return is at the end of the block
checkReturnAtEnd :: Statement -> Bool
checkReturnAtEnd (Block stmts) = case last stmts of
                  Return _ -> True
                  _ -> False
checkReturnAtEnd _ = False

--check number of parameter in function call and return statement is the same
checkNumPara :: Expression -> Statement -> Bool
checkNumPara (FunctionCall (VariableAssignment _ (Parameter x))) (Block stmts) =
  case last stmts of --last -> get the last statement in the list
    Return (FunctionCall (VariableAssignment _ (Parameter y))) -> length x == length y 
    _ -> False
checkNumPara _ _ = False

-- Check if a function is tail-recursive according to the given criteria
--take parse result and return boolean
isTailRecursive' :: ParseResult Expression -> Bool
isTailRecursive' (Result _ (FunctionCallBlock (FunctionCall (VariableAssignment name (Parameter x))) (Block stmts))) =
  critiria1 (FunctionCallBlock (FunctionCall (VariableAssignment name (Parameter x))) (Block stmts)) &&
  checkFunctionStatement (Block stmts) &&
  checkOneReturn name (Block stmts) &&
  checkReturnAtEnd (Block stmts) &&
  checkNumPara (FunctionCall (VariableAssignment name (Parameter x))) (Block stmts)
isTailRecursive' _ = False

-- Check if a function is tail-recursive according to the given criteria
-- parse s for isTailRecursive' to check if it is tail recursive
isTailRecursive :: String -> Bool
isTailRecursive s = isTailRecursive' (parse parsePartC s)

-- pretty print all the function
prettyPrintFunction :: Expression -> String
prettyPrintFunction (FunctionCall (Assignment name value)) =
  prettyPrintStatementHead (Assignment name value) 0 (isMultilineStatement [Assignment name value])

prettyPrintFunction (FunctionCall (VariableAssignment name (Parameter x))) =
  prettyPrintStatementHead (VariableAssignment name (Parameter x)) 0
  (isMultilineStatement [VariableAssignment name (Parameter x)]) ++  ";"  

prettyPrintFunction (FunctionCallBlock (FunctionCall (VariableAssignment name (Parameter x))) (Block stmts)) =
  "function " ++ name ++ "(" ++ prettyPrintExpressions x ++ ") " ++
  prettyPrintStatementHead (Block stmts) 0 (isMultilineStatement stmts) --check is multiline before prettyPrint the block

prettyPrintFunction _ = ""

-- pretty print tail recursive
prettyPrintTailRecursive :: Expression -> String
prettyPrintTailRecursive (FunctionCallBlock (FunctionCall (VariableAssignment name (Parameter x))) (Block stmts)) =
  "function " ++ name ++ "(" ++ prettyPrintExpressions x ++ ") " ++ prettyPrintBlockTailRecursive stmts x
prettyPrintTailRecursive _ = ""

-- pretty print block of tail recursive
-- init-> remove the last statement then concat it with another form of pretty print return statement
prettyPrintBlockTailRecursive :: [Statement] -> [Expression] -> String
prettyPrintBlockTailRecursive stmts e = "{\n\twhile ( true ) {\n" ++
  prettyPrintStatementMultiCheck (init stmts) 2 True ++
  "\n\t\t" ++ prettyPrintRetTailRecur (last stmts) e  ++ "\n\n\t}\n\n}"

-- pretty print expressions
prettyPrintExpressions :: [Expression] -> String
prettyPrintExpressions [] = ""
prettyPrintExpressions [x] = prettyPrintExpression x
prettyPrintExpressions (x:xs) = prettyPrintExpression x ++ ", " ++ prettyPrintExpressions xs

-- pretty print return statement for tail recursive
prettyPrintRetTailRecur :: Statement -> [Expression] -> String
prettyPrintRetTailRecur (Return (FunctionCall (VariableAssignment name expre))) e = "[" ++
  prettyPrintExpressions e ++ "] = " ++ "[" ++ prettyPrintExpression expre ++ "];"

prettyPrintRetTailRecur _ _ = ""

--wrap with expression
parseExerciseC :: Parser ADT
parseExerciseC = Expression <$> parsePartC

--pretty print function
--if it is a tail recursive then print it in tail recursive form
prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC (Expression (FunctionCallBlock x y)) = 
  if (isTailRecursive' (parse parsePartC (prettyPrintFunction (FunctionCallBlock x y))))
  then prettyPrintTailRecursive (FunctionCallBlock x y) 
  else prettyPrintFunction (FunctionCallBlock x y)
--if is other function then print it as a normal function
prettyPrintExerciseC (Expression e) = prettyPrintFunction e
prettyPrintExerciseC _ = ""

-- | Exercise E
-- Parser for ifThenElse statements
parseIfThenElse :: Parser Statement
parseIfThenElse = do
  _ <- spaces
  _ <- stringTok "if"
  _ <- op '('
  ifExpr <- parsePartA <|> parseMultiVariable -- if condition
  _ <- op ')'
  _ <- spaces
  _ <- stringTok "then"
  _ <- spaces
  thenExpr <- parseTernaryExpr -- then expression
  _ <- spaces
  _ <- stringTok "else"
  _ <- spaces
  elseExpr <- parseTernaryExpr --else expression
  _ <- spaces
  pure (IfThenElse ifExpr thenExpr elseExpr)

-- parse empty expression
parseEmpty :: Parser Expression
parseEmpty = spaces >> is '{' >> is '}' >> EmptyExpression <$ spaces

--Parser return for ifThenElse
parseRet :: Parser Expression
parseRet = Ret <$> parseReturn

-- parser used for convert if then else to ternary expression
parseTernaryExpr :: Parser Expression
parseTernaryExpr = asum[parseRet, parseEmpty, parsePartA]

--parse many parseIfThenElse
parseExerciseE :: Parser ADT
parseExerciseE = Statements <$> many parseIfThenElse

--convert if then else to ternary expression 
--calling pretty print expression ternary
convertTernary :: Statement -> String
convertTernary (IfThenElse ifExpr thenExpr elseExpr) = prettyPrintExpression (Ternary ifExpr thenExpr elseExpr)
convertTernary _ = ""

--pretty print ternary expression
prettyPrintExerciseE :: ADT -> String
prettyPrintExerciseE (Statements stmts) = unwords $ map convertTernary stmts
prettyPrintExerciseE _ = ""





