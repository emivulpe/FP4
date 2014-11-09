
--
-- This file is changed and I don't think it requires more changes for now!!! 
--

module F95SrcIO (
		read_F95_src,
        write_F95_src
) where
import Data.Char (toLower)
-- Fortran is case-insensitive so turn everything into lowercase
lc = map toLower
-- given the name of the file, read it into a list of strings, one per line of source code
read_F95_src :: String -> IO [String]


--
--changed 
--

read_F95_src src_name = do
    contents <- readFile src_name
    return foldr (\x acc -> lc x : acc) [] (lines contents)
    
    
--
--changed 
--
  
-- given a list of strings, one per line of source code, and the name of the file, write the strings to the file
write_F95_src :: String -> [String] -> IO ()
write_F95_src src_name src_lines =  do
    handle <- openFile src_name WriteMode  
    printToFile src_lines handle
    hClose handle
    
   
printToFile :: [String] -> Handle -> IO()
printToFile [] _ = return ()
printToFile [x] h = do hPutStr h (lc x)
printToFile (x:xs) h =do hPutStrLn h (lc x)
			 printToFile xs h

