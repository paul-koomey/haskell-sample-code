main = do   
    return ()
    return "hahaha"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line


--import System.IO
--main = do 
--    withFile "file.txt" ReadMode (\handle -> do
--        contents <- hGetContents handle
--        putStr contents)


