module Main where
import System.Environment
import System.FilePath (takeFileName)
import System.IO
import Control.Monad
import Data.Bits (xor, shiftL, shiftR, complement)
import Data.Char (isSpace, isDigit, toLower)
import Data.Fixed (mod')
import Data.List (transpose, intercalate)

data Value = VInt Int | VFloat Float | VBool Bool | VStr String | VVector [Int] | VMatrix [[Int]]
            | VQuoting String | VLambda String
  deriving (Eq)
instance Show Value where
  show (VInt i)  = show i
  show (VFloat f)  = show f
  show (VBool b) = map toLower (show b)
  show (VStr s)  = "\"" ++ s ++ "\""
  show (VVector v) = formatList v
  show (VMatrix m) = "[" ++ intercalate ", " (map formatList m) ++ "]"
  show (VQuoting q) = q
  show (VLambda l) = l

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  let input = head args
      inputFile = takeFileName input
      tokens = tokenize contents "" False False 0
  mapM_ putStrLn tokens
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

-- more helper functions
count c = length . filter (== c)
formatList xs = "[" ++ intercalate ", " (map show xs) ++ "]"