module Main where
import System.Environment
import Data.List (intercalate)
import System.FilePath (takeFileName)

main :: IO ()
main = do 
    args <- getArgs
    let inputFile = head args
        inputFileName = takeFileName inputFile
        outputFile = "output-" ++ drop 6 inputFileName
    contents <- readFile inputFile
    let result = process contents
    writeFile outputFile result

process :: String -> String
process input = intercalate "\n" $ interpret [] (words input)

interpret :: [String] -> [String] -> [String]
interpret stack [] = reverse stack
interpret stack (token:tokens) = 
    case token of
        -- Arithmetic operations
        "+"  -> interpret (performOp (+) stack) tokens
        "-"  -> interpret (performOp (-) stack) tokens
        "*"  -> interpret (performOp (*) stack) tokens
        "/"  -> interpret (performOp div stack) tokens
        "**" -> interpret (performOp (^) stack) tokens
        "%"  -> interpret (performOp mod stack) tokens
        
        -- Stack operations
        "DROP"  -> interpret (dropOp stack) tokens
        "DUP"   -> interpret (dupOp stack) tokens
        "SWAP"  -> interpret (swapOp stack) tokens
        "ROT"   -> interpret (rotOp stack) tokens
        "ROLL"  -> interpret (rollOp stack) tokens
        "ROLLD" -> interpret (rollDOp stack) tokens
        "IFELSE"-> interpret (ifElseOp stack) tokens
        
        -- Comparison operators
        "==" -> interpret (performCmp (==) stack) tokens
        "!=" -> interpret (performCmp (/=) stack) tokens
        ">"  -> interpret (performCmp (>) stack) tokens
        "<"  -> interpret (performCmp (<) stack) tokens
        ">=" -> interpret (performCmp (>=) stack) tokens
        "<=" -> interpret (performCmp (<=) stack) tokens
       -- "<=>" -> interpret (performCmp compare stack) tokens
        
        -- Boolean operators
        "&"  -> interpret (performBoolOp (&&) stack) tokens
        "|"  -> interpret (performBoolOp (||) stack) tokens
        "^" -> interpret (performBoolOp xor stack) tokens
        "!"  -> interpret (performNotOp stack) tokens
        
        -- Boolean values
        "true"  -> interpret ("true":stack) tokens
        "false" -> interpret ("false":stack) tokens
        
        -- Bitshift operators
        "<<" -> interpret (performShiftOp shiftL stack) tokens
        ">>" -> interpret (performShiftOp shiftR stack) tokens
        
        -- Default case
        num -> interpret (num:stack) tokens
    where
        shiftL x y = x * (2 ^ y)
        shiftR x y = x `div` (2 ^ y)

-- Helper functions
performOp :: (Int -> Int -> Int) -> [String] -> [String]
performOp op (y:x:stack) = 
    case (reads x, reads y) of
        ([(xNum, "")], [(yNum, "")]) -> show (op xNum yNum) : stack
        _ -> stack
performOp _ stack = stack

dropOp :: [String] -> [String]
dropOp (_:stack) = stack
dropOp stack = stack

dupOp :: [String] -> [String]
dupOp (x:stack) = x:x:stack
dupOp stack = stack

swapOp :: [String] -> [String]
swapOp (y:x:stack) = x:y:stack
swapOp stack = stack

rotOp :: [String] -> [String]
rotOp (z:y:x:stack) = x:z:y:stack
rotOp stack = stack

rollOp :: [String] -> [String]
rollOp (n:stack) = 
    case reads n of
        [(num, "")] -> 
            if num > 0 && num <= length stack
            then let (toRotate, rest) = splitAt num stack
                 in (last toRotate : init toRotate) ++ rest
            else stack
        _ -> stack
rollOp stack = stack

rollDOp :: [String] -> [String]
rollDOp (n:stack) = 
    case reads n of
        [(num, "")] -> 
            if num > 0 && num <= length stack
            then let (toRotate, rest) = splitAt num stack
                 in (tail toRotate ++ [head toRotate]) ++ rest
            else stack
        _ -> stack
rollDOp stack = stack

performCmp :: (Int -> Int -> Bool) -> [String] -> [String]
performCmp cmp (y:x:stack) = 
    case (reads x, reads y) of
        ([(xNum, "")], [(yNum, "")]) -> 
            if cmp xNum yNum then "true":stack else "false":stack
        _ -> stack
performCmp _ stack = stack

performBoolOp :: (Bool -> Bool -> Bool) -> [String] -> [String]
performBoolOp op (y:x:stack) = 
    case (x, y) of
        ("true", "true")   -> if op True True then "true":stack else "false":stack
        ("true", "false")  -> if op True False then "true":stack else "false":stack
        ("false", "true")  -> if op False True then "true":stack else "false":stack
        ("false", "false") -> if op False False then "true":stack else "false":stack
        _ -> stack
performBoolOp _ stack = stack

xor :: Bool -> Bool -> Bool
xor a b = a /= b

performNotOp :: [String] -> [String]
performNotOp ("true":stack) = "false":stack
performNotOp ("false":stack) = "true":stack
performNotOp stack = stack

ifElseOp :: [String] -> [String]
ifElseOp (cond:trueVal:falseVal:stack) = 
    case cond of
        "true"  -> trueVal:stack
        "false" -> falseVal:stack
        _ -> stack
ifElseOp stack = stack

performShiftOp :: (Int -> Int -> Int) -> [String] -> [String]
performShiftOp op (y:x:stack) = 
    case (reads x, reads y) of
        ([(xNum, "")], [(yNum, "")]) -> show (op xNum yNum) : stack
        _ -> stack
performShiftOp _ stack = stack