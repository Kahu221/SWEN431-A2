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
  OpNode "-" -> subOp stack
  OpNode "*" -> multOp stack
  OpNode "/" -> divOp stack
  OpNode "%" -> modOp stack
  OpNode "**" -> expOp stack
  OpNode "x" -> crossOp stack

  -- stack ops
  OpNode "SWAP" -> swapOp stack
  OpNode "DROP" -> dropOp stack
  OpNode "DUP" -> dupOp stack
  OpNode "ROT" -> rotOp stack
--  OpNode "ROLL" -> rollOp stack
  OpNode "ROLLD" -> rolldOp stack
  _          -> token : stack

rolldOp :: Stack -> Stack
rolldOp (IntNode n : xs)
  | n < 0 = error "ROLLD requires a non-negative integer"
  | length xs < n + 1 = error "Not enough elements on the stack for ROLLD"
  | otherwise =
      let (prefix, rest) = splitAt (n + 1) xs
      in case reverse prefix of
          (lastElem:restReversed) -> reverse restReversed ++ [lastElem] ++ rest
          _ -> error "Unexpected pattern in ROLLD"
rolldOp _ = error "ROLLD expects an integer followed by N+1 stack elements"


rotOp :: Stack -> Stack
rotOp (a:b:c:rest) = c:a:b:rest


dropOp :: Stack -> Stack
dropOp (_:xs) = xs

dupOp :: Stack -> Stack
dupOp (x:xs) = x:x:xs

swapOp :: Stack -> Stack
swapOp (x:y:rest) = y : x : rest

addOp :: Stack -> Stack
addOp (IntNode x : IntNode y : rest)     = IntNode (y + x) : rest
addOp (FloatNode x : FloatNode y : rest) = FloatNode (y + x) : rest
addOp (IntNode x : FloatNode y : rest)   = FloatNode (y + fromIntegral x) : rest
addOp (FloatNode x : IntNode y : rest)   = FloatNode (fromIntegral y + x) : rest

subOp :: Stack -> Stack
subOp (IntNode x : IntNode y : xs) = IntNode (y - x) : xs
subOp (FloatNode x : FloatNode y : xs) = FloatNode (y - x) : xs
subOp (IntNode x : FloatNode y : xs) = FloatNode (y - fromIntegral x) : xs
subOp (FloatNode x : IntNode y : xs) = FloatNode (fromIntegral y - x) : xs
subOp _ = error "Invalid operands for subtraction"

multOp :: Stack -> Stack
multOp (IntNode x : IntNode y : xs) = IntNode (y * x) : xs
multOp (FloatNode x : FloatNode y : xs) = FloatNode (y * x) : xs
multOp (IntNode x : FloatNode y : xs) = FloatNode (y * fromIntegral x) : xs
multOp (FloatNode x : IntNode y : xs) = FloatNode (fromIntegral y * x) : xs
multOp _ = error "Invalid operands for multiplication"

divOp :: Stack -> Stack
divOp (IntNode 0 : _) = error "Division by zero"
divOp (FloatNode 0.0 : _) = error "Division by zero"
divOp (IntNode x : IntNode y : xs) = IntNode (y `div` x) : xs
divOp (FloatNode x : FloatNode y : xs) = FloatNode (y / x) : xs
divOp (IntNode x : FloatNode y : xs) = FloatNode (y / fromIntegral x) : xs
divOp (FloatNode x : IntNode y : xs) = FloatNode (fromIntegral y / x) : xs
divOp _ = error "Invalid operands for division"

modOp :: Stack -> Stack
modOp (IntNode 0 : _) = error "Modulo by zero"
modOp (FloatNode 0.0 : _) = error "Modulo by zero"
modOp (IntNode x : IntNode y : xs) = IntNode (y `mod` x) : xs
modOp (FloatNode x : FloatNode y : xs) = FloatNode (mod' y x) : xs
modOp _ = error "Invalid operands for modulo"

expOp :: Stack -> Stack
expOp (IntNode x : IntNode y : xs) = IntNode (y ^ x) : xs
expOp (FloatNode x : FloatNode y : xs) = FloatNode (y ** x) : xs
expOp (IntNode x : FloatNode y : xs) = FloatNode (y ** fromIntegral x) : xs
expOp (FloatNode x : IntNode y : xs) = FloatNode (fromIntegral y ** x) : xs
expOp _ = error "Invalid operands for exponentiation"

-- create a vector node after crossing it and calulating the result array
crossOp :: Stack -> Stack
crossOp stack = case stack of
    (VectorNode [b1,b2,b3] : VectorNode [a1,a2,a3] : rest) ->
        VectorNode [ a2*b3 - a3*b2
                   , a3*b1 - a1*b3
                   , a1*b2 - a2*b1 ] : rest

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