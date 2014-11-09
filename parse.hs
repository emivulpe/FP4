import Text.ParserCombinators.Parsec

data Tag = MkTag String deriving (Show)

parseTag :: Parser ([Char],[Char],[Char])
parseTag = do
    x <- many meaningfulChar
    string "parameter"
    n <- many meaningfulChar
    v <- many digit
    return (x,n,v)
    
run :: Show a => Parser a -> String -> IO ()
run p input
  = case parse p "" input of
      Left err ->
        do putStr "parse error at "
           print err
      Right x -> print x

identifier :: Parser [Char]
identifier = do
     x <- many letter
     return x
     
listWhiteSpaceChars :: String
listWhiteSpaceChars = [' ', '\t', '\r']

meaningfulChar :: Parser Char
meaningfulChar = do 
    x <- noneOf ('\n' : ':' : ',' : listWhiteSpaceChars)
    return x

