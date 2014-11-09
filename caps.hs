import Control.Monad  
import Data.Char  
import System.IO  
import System.Environment   
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

data Tag = MkTag String deriving (Show)


{-
parseTag :: Parser Tag
parseTag = do
    char '<'
    x <- P.identifier
    char '>'
    return (MkTag x)

-}
lc = map toLower  --[Char] -> [Char]

-- file <- readFile src_name type: Fileath IO String
--lines file type: [String]
-- lc type: String -> String

main = do  
    handle <- openFile "test.txt" ReadMode  
    contents <- hGetContents handle  
    putStr "$ lc $ head $ lines $ contents"
    putStr $ head $ foldr (\x acc -> lc x : acc) [] (lines contents)
    hClose handle 
    write_F95_src "testwriting.txt" (foldr (\x acc -> lc x : acc) [] (lines contents))
    
    
read_F95_src :: String -> IO [String]
read_F95_src src_name = do
    contents <- readFile src_name
    return (foldr (\x acc -> lc x : acc) [] (lines contents))

write_F95_src :: String -> [String] -> IO ()
write_F95_src src_name src_lines =  do
    handle <- openFile src_name WriteMode 
    --hPutStr handle $ head $ src_lines 
    printToFile src_lines handle
    hClose handle
    
   
printToFile :: [String] -> Handle -> IO()
printToFile [] _ = return ()
printToFile [x] h = do hPutStr h (lc x)
printToFile (x:xs) h =do hPutStrLn h (lc x)
			 printToFile xs h



run :: Show a => Parser a -> String -> IO ()
run p input
  = case parse p "" input of
      Left err ->
        do putStr "parse error at "
           print err
      Right x -> print x
      
      		 
{=		 
f95_par_decl_parser = do
	t <- P.identifier
	-- ignore , parameter ::
	char ','
	P.identifier
	char ':'
	char ':'
	name <- P.identifier 
	char '='
	val <- many digit
	-- pv <- parse val
	putStr t
	putStr name
	putStr val
	return (t,name,val)
-}		 
--readFromFile :: String -> 
