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
f95_par_decl_parser = do
    x <- varType
    skipNonMeaningful
    l <- string "parameter" --couldn't do it with string "parameter"
    skipNonMeaningful
    n <- many meaningfulChar
    skipNonMeaningful
    v <- many meaningfulChar
    let expr = read v :: Integer
    return $ MkParDecl x [] n $ Const expr
{-
f95_par_decl_parser = do
	pt <- identifier
	-- ignore , parameter ::
	pn <- identifier 
	-- pv <- parse val
	return $ MkParDecl pt [] pn pv
-}

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

