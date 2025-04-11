module Main where
import System.Environment
import System.IO()
import Data.List (intercalate)
import System.FilePath (takeFileName)

main :: IO ()
main = do 
    args <- getArgs
    let inputFile = head args
        inputFileName = takeFileName inputFile  -- Extracts the file name (e.g., "input-001.txt")
        outputFile = "output-" ++ drop 6 inputFileName  -- Extracts "001" from "input-001.txt"
    contents <- readFile inputFile
    let result = process contents
    writeFile outputFile result

-- Process the input and return the stack as newline-separated strings
process :: String -> String
process input = intercalate "\n" $ interpret [] (words input)

-- Interpret the input tokens and return the final stack
interpret :: [String] -> [String] -> [String]
interpret stack [] = reverse stack  -- Base case: no more tokens, return stack
interpret stack (token:tokens) = 
    case token of
        -- Arithmetic operations
        "+" -> interpret (performOp (+) stack) tokens
        "-" -> interpret (performOp (-) stack) tokens
        "*" -> interpret (performOp (*) stack) tokens
        "/" -> interpret (performOp div stack) tokens
        
        -- Stack operations
        "DROP" -> interpret (dropOp stack) tokens
        "DUP"  -> interpret (dupOp stack) tokens
        "SWAP" -> interpret (swapOp stack) tokens
        
        -- Default case: push number to stack
        num -> interpret (num:stack) tokens

-- Helper function for binary operations
performOp :: (Int -> Int -> Int) -> [String] -> [String]
performOp op (y:x:stack) = show (op (read x) (read y)) : stack
performOp _ stack = stack  -- Handle error cases (not required per assignment)

-- Stack operation helpers
dropOp :: [String] -> [String]
dropOp (_:stack) = stack
dropOp stack = stack

dupOp :: [String] -> [String]
dupOp (x:stack) = x:x:stack
dupOp stack = stack

swapOp :: [String] -> [String]
swapOp (y:x:stack) = x:y:stack
swapOp stack = stack
