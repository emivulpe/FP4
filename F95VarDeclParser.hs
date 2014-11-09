module F95VarDeclParser where
import F95Types
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

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


--code for type think it is ready. Ask for the additional methods and meaning of kind      
type_parser :: Parser VarType
type_parser = do
     x <- many meaningfulChar
     skipNonMeaningful --skip "("
     many meaningfulChar --skip "kind"
     skipNonMeaningful
     kind <- integer
     return MkVarType { at_numtype = getNumType x , at_wordsz = kind}
  
getNumType :: String -> NumType
getNumType "integer" = F95Integer
getNumType "real" = F95Real  


--end of code for type






  
dim_parser :: Parser [Range]
dim_parser = return [dummyRange]

range_parser :: Parser Range
range_parser = return dummyRange

single_var_range :: Parser Range    
single_var_range = do
	many meaningfulChar --skip dimension
	skipNonMeaningful
	var <- many meaningfulChar
	skipNonMeaningful
	return $ MkRange {r_start = Var var, r_stop = Var var}

single_const_range :: Parser Range
single_const_range = do
	many meaningfulChar --skip dimension
	skipNonMeaningful
	const <- integer
	skipNonMeaningful
	return $ MkRange {r_start = Const const, r_stop = Const const}

single_expr_range :: Parser Range
single_expr_range = return dummyRange

range_expr :: Parser Range    
range_expr =  return dummyRange





--code for intent. Think it is ready and works
intent_parser :: Parser Intent    
intent_parser = do
	many meaningfulChar --skip "intent"
	skipNonMeaningful --skip "("
	intent <- many meaningfulChar
	skipNonMeaningful
	return $ getIntent intent


getIntent :: String -> Intent
getIntent "in" = In
getIntent "out" = Out
getIntent "inout" = InOut

--end of code for intent



--code for parsing the list of varnames. Works
arglist_parser :: Parser [VarName]    
arglist_parser = do
	first <- many meaningfulChar
	next <- remainingArgs
	return (first : next)
	
remainingArgs = do
	(char ',' >> arglist_parser)
	<|> (return [])
	

--end of code for parsing list of varnames 

--code to parse ocl argmode. works

listPragmaChars :: [Char]
listPragmaChars = ['!', '$']

pragmas :: Parser ()
pragmas = do
	ps <- many $ oneOf listPragmaChars
	return ()


ocl_argmode_parser :: Parser OclArgMode    
ocl_argmode_parser = do
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
	
--end of code to parse ocl argmode




	
-- Parser for a term in expression as used e.g. in the dimension() attribute. 
-- This is not a dummy
term :: Parser Expr
term = parens expr_parser <|> const_expr <|> var_expr <?> "simple expression"
      
-- Parser for an expression as used e.g. in the dimension() attribute. 
-- This is not a dummy
expr_parser :: Parser Expr
expr_parser = buildExpressionParser optable term <?> "expression"





-- code for parser for a constant, e.g. 42. Works

const_expr :: Parser Expr
const_expr = do
	const <- integer
	return $ Const const

--end of code for parser for a constant

-- code for parser for a variable e.g. v. Works
var_expr :: Parser Expr
var_expr = do
	var <- many meaningfulChar
	return $ Var var

-- end of the code for parser for a variable



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



--I added this. Can I leave it here


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
	
	
	