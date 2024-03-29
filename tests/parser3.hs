module F95ParDeclParser 
where
import F95Types
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import F95VarDeclParser

-- parse a parameter declaration string into a ParDecl 
f95_par_decl_parser :: Parser ParDecl
f95_par_decl_parser = return $ MkParDecl dummyVarType [] dummyVarName dummyExpr

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
     kind <- many integer
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


