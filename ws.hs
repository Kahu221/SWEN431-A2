module Main where 
import System.Environment
import System.IO  
import Control.Monad

main = do 
        args <- getArgs
        contents <- readFile (head args)
        writeFile "output.txt" (process contents) -- static output file name just for demonstration     

process input = "START\n" ++ input ++ "\nEND"
