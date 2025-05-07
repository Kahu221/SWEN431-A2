module Main where 
import System.Environment
import System.IO
import Control.Monad
import Data.Bits
import Data.Char (digitToInt, isSpace, isPrint, isDigit, toLower)
import Text.Parsec.Expr (Operator)
import Data.List (isInfixOf, intersperse, transpose, isPrefixOf, intercalate)
import Text.Read (Lexeme(String))
import Data.Functor.Reverse (Reverse)
import System.Posix (OpenFileFlags(creat))
import System.FilePath (takeBaseName, replaceExtension)

data Node = IntNode Int          
           | FloatNode Float     
           | BoolNode Bool      
           | StringNode String     
           | VectorNode [Int]   
           | MatrixNode [[Int]]   
           | QuotedNode String   
           | LambdaNode String  
           deriving (Eq)  

instance Show Node where
  show (IntNode i)  = show i
  show (FloatNode f)  = show f
  show (BoolNode b) = map toLower (show b)
  show (StringNode s)  = "\"" ++ s ++ "\""
  show (VectorNode v) = formatList v
  show (MatrixNode m) = "[" ++ intercalate ", " (map formatList m) ++ "]"
  show (QuotedNode q) = q
  show (LambdaNode l) = l

formatList xs = "[" ++ intercalate ", " (map show xs) ++ "]"


type Stack = [Node]

main :: IO ()
main = do 
    args <- getArgs
    case args of
        [] -> putStrLn "Error: No input file provided"
        (inputFile:_) -> do
            contents <- readFile inputFile
            -- extract 3 digits from the input file name
            let baseName = takeBaseName inputFile
                outputDigits = take 3 $ dropWhile (not . isDigit) baseName
                outputFile = "output-" ++ outputDigits ++ ".txt"
            
            -- For testing tokenizer during development
            let tokens = tokenize contents "" False False 0
            putStrLn $ "Tokens: " ++ show tokens  -- Remove this line before submission
            
            -- Process the input and write to properly named output file
            writeFile outputFile (unlines tokens)  -- Using your tokenizer for now
            putStrLn $ "Output written to " ++ outputFile  -- Remove this line before submission

-- Your exact tokenize function (unchanged as requested)
tokenize :: [Char] -> String -> Bool -> Bool -> Int -> [String]
tokenize [] s _ _ _ 
    | null s = []
    | otherwise = [reverse s]
tokenize (o:ox) s quoted brackted count 
    | '{' == o && not quoted = tokenize ox (o : s) quoted (not brackted) count
    | '}' == o && not quoted = tokenize ox (o : s) quoted (not brackted) count
    | brackted = tokenize ox (o : s) quoted brackted count
    -- if it's an open square bracket increment it 
    | '[' == o && not quoted = tokenize ox (o : s) quoted brackted (count + 1)
    -- if it's a closed square bracket decrement it
    | ']' == o && not quoted = tokenize ox (o : s) quoted brackted (count - 1)
    -- if it's going to be a string keep creating a token until not quoted
    | count > 0 = tokenize ox (o : s) quoted brackted count
    | '"' == o = tokenize ox (o : s) (not quoted) brackted count
    -- If we're still quoted just keep creating the token
    | quoted = tokenize ox (o : s) quoted brackted count
    -- if it's not a space keep creating the token
    | not (isSpace o) = tokenize ox (o : s) quoted brackted count
    | not (null s) = reverse s : tokenize ox [] quoted brackted count
    -- if it's a space, skip it 
    | otherwise = tokenize ox [] quoted brackted count