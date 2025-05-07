module Main where
import System.Environment
import System.IO
import Control.Monad
import Data.Bits (xor, shiftL, shiftR, complement)
import Data.Char (isSpace, isDigit, toLower)
import Data.Fixed (mod')
import Data.List (isPrefixOf, isSuffixOf, intercalate, transpose)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

data Node = IntNode Int
          | OpNode String
          | FloatNode Float
          | BoolNode Bool
          | StrNode String
          | VectorNode [Int]
          | MatrixNode [[Int]]
          | QuoteNode String
          | LambdaNode String
          deriving (Eq)
type Stack = [Node]

instance Show Node where
  show (OpNode op)    = op
  show (IntNode i)    = show i
  show (FloatNode f)  = show f
  show (BoolNode b)   = map toLower (show b)
  show (StrNode s)    = "\"" ++ s ++ "\""
  show (VectorNode v) = formatList v
  show (MatrixNode m) = "[" ++ intercalate ", " (map formatList m) ++ "]"
  show (QuoteNode q)  = q
  show (LambdaNode l) = l

-- helper stuff for printing TODO remove
showNodeType :: Node -> String
showNodeType (IntNode _)    = "Int"
showNodeType (FloatNode _)  = "Float"
showNodeType (BoolNode _)   = "Bool"
showNodeType (StrNode _)    = "String"
showNodeType (VectorNode _) = "Vector"
showNodeType (MatrixNode _) = "Matrix"
showNodeType (QuoteNode _)  = "Quote"
showNodeType (LambdaNode _) = "Lambda"
showNodeType (OpNode _) = "Operator"

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  let input = head args
      inputFile = reverse (takeWhile (/= '/') (reverse input)) -- gets filename from path
      outputFile = "output-" ++ drop 6 inputFile
      tokens = tokenize contents "" False False 0
      castedTokens = map castToken tokens
      result = process castedTokens
  writeFile outputFile (unlines (map show (reverse result)))

--        mapM_ (\node -> putStrLn $ "Token: " ++ show node ++ " :: " ++ showNodeType node) castedTokens
process :: Stack -> Stack
process tokens = foldl applyToken [] tokens

applyToken :: Stack -> Node -> Stack
applyToken stack token = case token of
  OpNode "+" -> addOp stack
  _          -> token : stack

addOp :: Stack -> Stack
addOp (IntNode x : IntNode y : rest)     = IntNode (y + x) : rest
addOp (FloatNode x : FloatNode y : rest) = FloatNode (y + x) : rest
addOp (IntNode x : FloatNode y : rest)   = FloatNode (y + fromIntegral x) : rest
addOp (FloatNode x : IntNode y : rest)   = FloatNode (fromIntegral y + x) : rest
addOp stack = error $ "Invalid stack for addOp: " ++ show stack


castToken token
  | isLambda token     = LambdaNode token
  | isQuoted token     = QuoteNode (tail token)
  | isMatrix token     = MatrixNode (parseMatrix token)
  | isVector token     = VectorNode (parseVector token)
  | isString token     = StrNode (init (tail token))
  | isBool token       = BoolNode (toBool token)
  | isInteger token    = IntNode (read token)
  | isFloat token      = FloatNode (read token)
  | isOp token         = OpNode token
  | otherwise          = StrNode token
  where
    isLambda s = "{" `isPrefixOf` s && "}" `isSuffixOf` s
    isQuoted s = "'" `isPrefixOf` s
    isMatrix s = "[[" `isPrefixOf` s && "]]" `isSuffixOf` s
    isVector s = "[" `isPrefixOf` s && "]" `isSuffixOf` s && not (isMatrix s)
    isString s = "\"" `isPrefixOf` s && "\"" `isSuffixOf` s
    isBool s = case map toLower s of
                   "true"  -> True
                   "false" -> True
                   _       -> False
    isOp s = s `elem` [ "+", "-", "*", "/", "%", "**", "x", "==", "!=", "<", ">", "<=", ">=", "<=>", "&", "|", "^", "<<", ">>", "!", "~", "DROP", "DUP", "SWAP", "ROT", "ROLL", "ROLLD", "IFELSE", "TRANSP", "EVAL"]
    isInteger s = case reads s :: [(Int, String)] of [(_, "")] -> True; _ -> False
    isFloat s = case reads s :: [(Float, String)] of [(_, "")] -> True; _ -> False

    toBool s = case map toLower s of
                 "true"  -> True
                 "false" -> False
-- Helpers to safely parse vector/matrix
parseVector :: String -> [Int]
parseVector s = fromMaybe [] (readMaybe s :: Maybe [Int])

parseMatrix :: String -> [[Int]]
parseMatrix s = fromMaybe [] (readMaybe s :: Maybe [[Int]])


-- TODO : ADJUST THIS
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