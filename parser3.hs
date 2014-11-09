module F95VarDeclParser where
import F95Types
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language


{-


data VarDecl = MkVarDecl {
	vd_vartype :: VarType
	,vd_dimension :: [Range]	
	,vd_intent :: Intent
	,vd_varlist :: [VarName]
	,vd_argmode :: OclArgMode
    ,vd_is_arg :: Bool
} deriving (Eq, Ord, Show)

data Intent = In | Out | InOut  deriving (Eq, Ord, Show)

data VarType = MkVarType {
	at_numtype :: NumType,
	at_wordsz :: Integer
}  deriving (Eq, Ord, Show)

data NumType =  F95Integer | F95Real  deriving (Eq, Ord, Show)

data OclArgMode = Read | Write | ReadWrite 
	deriving (Eq, Ord, Show)

data Range = MkRange {
		r_start :: Expr
		,r_stop :: Expr
}  deriving (Eq, Ord, Show)

data Expr = Var VarName | Const Integer | Op OpExpr | Pref PrefixOpExpr  deriving (Eq, Ord, Show)

type VarName = String

data OpExpr = MkOpExpr {
	oe_op :: String
   ,oe_lhs :: Expr
   , oe_rhs :: Expr
}  deriving (Eq, Ord, Show)
data PrefixOpExpr = MkPrefixOpExpr {
	poe_op :: String
   ,poe_exp :: Expr
}  deriving (Eq, Ord, Show)


-}



-- Run a parser p on a string str and print the result
run_parser_print :: Show a => Parser a -> String -> IO ()
run_parser_print p str = do
      case parse p "" str of
           Left err -> do
               putStr "parse error at "
               print err
           Right x  -> putStrLn $ "    "++(show x)++","
                                                                                                                                                         
-- Run a parser p on a string str and return the result
run_parser :: Parser a -> String -> a
run_parser p str =  case parse p "" str of
    Left err -> error $ "parse error at " ++ (show err)
    Right val  -> val  

f95_var_decl_parser :: Parser VarDecl
f95_var_decl_parser = return dummyVarDecl
      
type_parser :: Parser VarType
type_parser = return dummyVarType
      
dim_parser :: Parser [Range]
dim_parser = return [dummyRange]

range_parser :: Parser Range
range_parser = return dummyRange

single_var_range :: Parser Range    
single_var_range = return dummyRange

single_const_range :: Parser Range
single_const_range = return dummyRange

single_expr_range :: Parser Range
single_expr_range = return dummyRange

range_expr :: Parser Range    
range_expr =  return dummyRange

intent_parser :: Parser Intent    
intent_parser = return dummyIntent
   
arglist_parser :: Parser [VarName]    
arglist_parser = return [dummyVarName]

ocl_argmode_parser :: Parser OclArgMode    
ocl_argmode_parser = return dummyArgMode

-- Parser for a term in expression as used e.g. in the dimension() attribute. 
-- This is not a dummy
term :: Parser Expr
term = parens expr_parser <|> const_expr <|> var_expr <?> "simple expression"
      
-- Parser for an expression as used e.g. in the dimension() attribute. 
-- This is not a dummy
expr_parser :: Parser Expr
expr_parser = buildExpressionParser optable term <?> "expression"

-- parser for a constant, e.g. 42
const_expr :: Parser Expr
const_expr = return dummyConstExpr

-- parser for a variable e.g. v
var_expr :: Parser Expr
var_expr = return dummyVarExpr

-- I suggest you don't touch the code below. It is not dummy code.
optable =
    let
        binop name assoc   = Infix ( do {  reservedOp name; return (\x y ->(Op (MkOpExpr name x y))) } ) assoc
        prefix name     = Prefix  ( reservedOp  name >> return (\x ->(Pref (MkPrefixOpExpr name x))) ) 
    in
        [ 
          [ prefix "-" ],
          [ binop "*"  AssocLeft, binop "/"  AssocLeft, binop "%" AssocLeft ]
        , [ binop "+"  AssocLeft, binop "-"  AssocLeft ]
        ]

lexer       = P.makeTokenParser emptyDef    

parens          = P.parens lexer    
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
word            = P.identifier lexer
identifier      = P.identifier lexer
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer    
charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer    
comma           = P.comma lexer
semi            = P.semi lexer
natural         = P.natural lexer
operator        = P.operator lexer



parseTag :: Parser ParDecl
parseTag = do
    x <- varType
    skipNonMeaningful
    l <- string "parameter" --couldn't do it with string "parameter"
    skipNonMeaningful
    n <- many meaningfulChar
    skipNonMeaningful
    v <- many meaningfulChar
    let expr = read v :: Integer
    return $ MkParDecl x [] n $ Const expr
    
run :: Show a => Parser a -> String -> IO ()
run p input
  = case parse p "" input of
      Left err ->
        do putStr "parse error at "
           print err
      Right x -> print x

varType :: Parser VarType
varType = do
     x <- many meaningfulChar
     skipNonMeaningful --skip "("
     many meaningfulChar --skip "kind"
     skipNonMeaningful
     kind <- integer
     return MkVarType { at_numtype = getNumType x , at_wordsz = kind} --at_wordsz is not correct- ask!!!!
     
     
getNumType :: String -> NumType
getNumType "integer" = F95Integer
getNumType "real" = F95Real  

listNonMeaningfulChars :: String
listNonMeaningfulChars = ['\n', '=',':', ',', ' ', '\t', '\r', '(',')']

meaningfulChar :: Parser Char
meaningfulChar = do 
    x <- noneOf listNonMeaningfulChars
    return x
skipNonMeaningful :: Parser ()
skipNonMeaningful = do
	xs <- many $ oneOf listNonMeaningfulChars
	return ()

{-
range :: Parser Range
range = do
	many meaningfulChar --skip dimension
	skipNonMeaningful --skip "("
	start <- integer
	skipNonMeaningful -- skip ","

-}
intent :: Parser Intent
intent = do
	many meaningfulChar --skip "intent"
	skipNonMeaningful --skip "("
	intent <- many meaningfulChar
	skipNonMeaningful
	return $ getIntent intent
	
getIntent :: String -> Intent
getIntent "in" = In
getIntent "out" = Out
getIntent "inout" = InOut


vars = do
	first <- many meaningfulChar
	next <- remainingVars
	return (first : next)

remainingVars = do
	(char ',' >> vars)
	<|> (return [])
	
listPragmaChars :: [Char]
listPragmaChars = ['!', '$']

pragmas :: Parser ()
pragmas = do
	ps <- many $ oneOf listPragmaChars
	return ()
	
mode :: Parser OclArgMode
mode = do
	pragmas --skip "!$"
	many meaningfulChar --skip ACC
	skipNonMeaningful --skip ' '
	string "argmode" --skip Argmode
	skipNonMeaningful --skip ' '
	mode <- many meaningfulChar
	return $ getOclArgMode mode
	
getOclArgMode :: String -> OclArgMode
getOclArgMode "read" = Read
getOclArgMode "write" = Write
getOclArgMode "readwrite" = ReadWrite

{-
rangeParser Parser Range
rangeParser = do
	many meaningfulChar --skip dimension
	skipNonMeaningful --skip "("
	start <- 
	char ':'
-}



varRange :: Parser Range
varRange = do
	many meaningfulChar --skip dimension
	skipNonMeaningful
	var <- many meaningfulChar
	skipNonMeaningful
	return $ MkRange {r_start = Var var, r_stop = Var var}
	
	
constRange :: Parser Range
constRange = do
	many meaningfulChar --skip dimension
	skipNonMeaningful
	const <- integer
	skipNonMeaningful
	return $ MkRange {r_start = Const const, r_stop = Const const}
	

exprRange :: Parser Range
exprRange = do
	many meaningfulChar --skip dimension
	skipNonMeaningful
	lhs <- term
	op <- operator
	rhs <- term
	skipNonMeaningful
	let opExpr = MkOpExpr { oe_op = op, oe_lhs = Const 14, oe_rhs = Var "d2"}
	return $ MkRange {r_start = Op opExpr, r_stop = Op opExpr}

	
constParser :: Parser Expr
constParser = do
	const <- integer
	return $ Const const
	
varParser :: Parser Expr
varParser = do 
	var <- many meaningfulChar
	return $ Var var
	
	
	
	
singleExprRange :: Parser Expr
singleExprRange = do
	---many meaningfulChar --skip dimension
	---skipNonMeaningful --skip "("
	t <- termParser
	expr <- exprParser
	term2 <-termParser
	let e = (expr t term2)
	return  e
	
	
	
	
termParser :: Parser Expr
termParser = parens exprParser <|> constParser <|> varParser <?> "simple expression"
      
-- Parser for an expression as used e.g. in the dimension() attribute. 
-- This is not a dummy
exprParser :: Parser Expr
exprParser = buildExpressionParser optable2 term <?> "expression"

-- I suggest you don't touch the code below. It is not dummy code.
optable2 =
    let
        binop name assoc   = Infix ( do {  reservedOp name; return (\x y ->(Op (MkOpExpr name x y))) } ) assoc
        prefix name     = Prefix  ( reservedOp  name >> return (\x ->(Pref (MkPrefixOpExpr name x))) ) 
    in
        [ 
          [ prefix "-" ],
          [ binop "*"  AssocLeft, binop "/"  AssocLeft, binop "%" AssocLeft ]
        , [ binop "+"  AssocLeft, binop "-"  AssocLeft ]
        ]