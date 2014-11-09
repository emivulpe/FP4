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
data ParDecl = MkParDecl {
	pd_partype :: VarType
	,pd_dimension :: [Range]	
	,pd_parname:: VarName
	,pd_parval:: Expr
} deriving (Eq, Ord, Show)



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
     return MkVarType { at_numtype = getNumType x , at_wordsz = 8} --at_wordsz is not correct- ask!!!!
     
     
getNumType :: String -> NumType
getNumType "integer" = F95Integer
getNumType "real" = F95Real  

listNonMeaningfulChars :: String
listNonMeaningfulChars = ['\n', '=',':', ',', ' ', '\t', '\r']

meaningfulChar :: Parser Char
meaningfulChar = do 
    x <- noneOf listNonMeaningfulChars
    return x
skipNonMeaningful :: Parser ()
skipNonMeaningful = do
	xs <- many $ oneOf listNonMeaningfulChars
	return ()


