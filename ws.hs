module Main where
import System.Environment
import System.IO
import Data.Bits (xor, shiftL, shiftR, complement)
import Data.Char (isSpace, isDigit, toLower)
import Data.List (isPrefixOf, isSuffixOf, intercalate, transpose)
-- TODO vector and matrix float stuff

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

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  let input = head args
      inputFile = reverse (takeWhile (/= '/') (reverse input))
      outputFile = "output-" ++ drop 6 inputFile
      tokens = parseStringToTokens contents "" False False 0
      castedTokens = map castToken tokens
      result = process castedTokens
  writeFile outputFile (unlines (map show (reverse result)))

process :: Stack -> Stack
process tokens = foldl applyToken [] tokens

applyToken :: Stack -> Node -> Stack
applyToken stack token = case token of
  LambdaNode lambda -> processLambda lambda stack
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
  OpNode "ROLL" -> rollOp stack
  OpNode "ROLLD" -> rolldOp stack
  OpNode "IFELSE" -> ifElseOp stack

  OpNode "TRANSP" -> transposeOp stack

  OpNode "EVAL" -> evalOp stack
  OpNode "^" -> xorOp stack
  OpNode "&" -> boolOp (&&) stack
  OpNode "|" -> boolOp (||) stack
  OpNode "==" -> cmpOp "==" stack
  OpNode "!=" -> cmpOp "!=" stack
  OpNode ">" -> cmpOp ">" stack
  OpNode "<" -> cmpOp "<" stack
  OpNode ">=" -> cmpOp ">=" stack
  OpNode "<=" -> cmpOp "<=" stack

  OpNode "<=>" -> cpmEqualityOp stack
  OpNode "<<" -> binIntOp shiftL stack
  OpNode ">>" -> binIntOp shiftR stack

  OpNode "~" -> unaryNumNodeOp complement stack
  OpNode "!" -> unaryBoolNodeOp not stack

  _          -> token : stack

processLambda :: String -> Stack -> Stack
processLambda lambda stack =
  let
    (cleanLambda, lambdaStack, rest) = getLambdaParams lambda stack
    resolvedBody = parseLambda cleanLambda lambdaStack
    tokens = parseStringToTokens resolvedBody "" False False 0
    castedTokens = map castToken tokens
    result = process castedTokens
  in result ++ rest

parseLambda:: String -> Stack -> String
parseLambda lambda s = unwords (map replace (words inner))
        where
        newStack = reverse s
        inner = init (tail lambda)
        replace ('x':ds) | all (`elem` ['0'..'9']) ds =
                let i = read ds in case drop i newStack of
                    (x:_) -> show x
                    []    -> "x" ++ ds
        replace token = token

getLambdaParams:: String -> Stack -> (String, Stack, Stack)
getLambdaParams lambda s = (cleanLambda, popped, rest)
        where
        n = read (takeWhile (/= '|') (filter (`notElem` " {}") lambda))
        popped = take n s
        rest = drop n s
        inner = drop 1 . take (length lambda - 1) $ lambda
        cleanBody = drop 1 ( dropWhile (/='|') inner )
        cleanLambda = "{ " ++ cleanBody ++ " }"



evalOp (x : rest) = foldl applyToken rest [castToken (show x)]

transposeOp :: Stack -> Stack
transposeOp (MatrixNode m : rest) = MatrixNode (transpose m) : rest
transposeOp (VectorNode v : rest) = MatrixNode [v] : rest

unaryNumNodeOp f stack =
  case stack of
    IntNode x : rest ->
      let result = f x
      in IntNode result : rest

unaryBoolNodeOp f stack =
  case stack of
    BoolNode x : rest ->
      let result = f x
      in BoolNode result : rest

binIntOp :: (Int -> Int -> Int) -> Stack -> Stack
binIntOp f (IntNode x : IntNode y : rest) =
  let result = f y x
      node = IntNode result
  in node : rest

cpmEqualityOp :: Stack -> Stack
cpmEqualityOp (x : y : rest) =
  let stringX = show x
      stringY = show y
      result = compare stringY stringX
      output = case result of
        LT -> IntNode (-1)
        EQ -> IntNode 0
        GT -> IntNode 1
  in output : rest

cmpOp :: String -> Stack -> Stack
cmpOp op stack =
  case stack of
    (y : x : rest) ->
      case (x, y) of
        (IntNode a, IntNode b)       -> BoolNode (compareInt op a b) : rest
        (FloatNode a, FloatNode b) -> BoolNode (compareFloat op a b) : rest
        (StrNode a, StrNode b) -> BoolNode (compareString op a b) : rest
        (BoolNode a, BoolNode b) -> BoolNode (compareBool op  a b) : rest
        (QuoteNode a, QuoteNode b) -> BoolNode (compareString op a b) : rest
        (LambdaNode a, LambdaNode b) -> BoolNode (compareString op a b) : rest

        (FloatNode a, IntNode b) -> BoolNode (compareFloat op a (fromIntegral b)) : rest
        (IntNode a, FloatNode b) -> BoolNode (compareFloat op (fromIntegral a) b) : rest

        (VectorNode a, VectorNode b) -> BoolNode (compareVector op a b) : rest
        (MatrixNode a, MatrixNode b) -> BoolNode (compareMatrix op a b) : rest

compareVector :: String -> [Int] -> [Int] -> Bool
compareVector "==" = (==)
compareVector "!=" = (/=)

compareMatrix :: String -> [[Int]] -> [[Int]] -> Bool
compareMatrix "==" = (==)
compareMatrix "!=" = (/=)

compareBool :: String -> Bool -> Bool -> Bool
compareBool "==" = (==)
compareBool "!=" = (/=)

compareInt :: String -> Int -> Int -> Bool
compareInt "==" = (==)
compareInt "!=" = (/=)
compareInt "<"  = (<)
compareInt ">"  = (>)
compareInt "<=" = (<=)
compareInt ">=" = (>=)

compareFloat :: String -> Float -> Float -> Bool
compareFloat "==" = (==)
compareFloat "!=" = (/=)
compareFloat "<"  = (<)
compareFloat ">"  = (>)
compareFloat "<=" = (<=)
compareFloat ">=" = (>=)

compareString :: String -> String -> String -> Bool
compareString "==" = (==)
compareString "!=" = (/=)
compareString "<"  = (<)
compareString ">"  = (>)
compareString "<=" = (<=)
compareString ">=" = (>=)

ifElseOp :: Stack -> Stack
ifElseOp (BoolNode cond : falseVal : trueVal : rest)
  | cond      = trueVal : rest
  | otherwise = falseVal : rest

xorOp :: Stack -> Stack
xorOp (IntNode x : IntNode y : rest)   = IntNode (y `xor` x) : rest -- Integer bitwise XOR
xorOp (BoolNode x : BoolNode y : rest) = BoolNode (y /= x) : rest -- Boolean logical XOR

boolOp op (BoolNode b1 : BoolNode b2 : rest) =
  BoolNode (b2 `op` b1) : rest

rolldOp (IntNode n : rest) =
  let
    (firstPart, bottom) = splitAt n rest
    x : xs = firstPart
    newList = xs ++ [x] ++ bottom
  in
    newList

rollOp (IntNode n : rest) =
  let (top, bottom) = splitAt n rest
      rotatedTop = last top : init top
  in rotatedTop ++ bottom

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
addOp (StrNode x : StrNode y : rest)   = StrNode (y ++ x) : rest
addOp (VectorNode x : VectorNode y : rest) = VectorNode (zipWith (+) y x) : rest

subOp :: Stack -> Stack
subOp (IntNode x : IntNode y : xs) = IntNode (y - x) : xs
subOp (FloatNode x : FloatNode y : xs) = FloatNode (y - x) : xs
subOp (IntNode x : FloatNode y : xs) = FloatNode (y - fromIntegral x) : xs
subOp (FloatNode x : IntNode y : xs) = FloatNode (fromIntegral y - x) : xs

multOp :: Stack -> Stack
multOp (IntNode x : IntNode y : xs) = IntNode (y * x) : xs
multOp (FloatNode x : FloatNode y : xs) = FloatNode (y * x) : xs
multOp (IntNode x : FloatNode y : xs) = FloatNode (y * fromIntegral x) : xs
multOp (FloatNode x : IntNode y : xs) = FloatNode (fromIntegral y * x) : xs
multOp (StrNode x : IntNode y : xs) = StrNode (concat (replicate y x)) : xs
multOp (IntNode x : StrNode y : xs) = StrNode (concat (replicate x y)) : xs
multOp (FloatNode x : StrNode y : xs) = StrNode (concat (replicate (round x) y)) : xs
multOp (StrNode x : FloatNode y : xs) = StrNode (concat (replicate (round y) x)) : xs
multOp (VectorNode x : VectorNode y : rest) = IntNode (sum (zipWith (*) y x)) : rest
multOp (MatrixNode x : MatrixNode y : rest) =
  let result = [[sum $ zipWith (*) row col | col <- transpose y] | row <- x]
  in MatrixNode result : rest


multOp (VectorNode x : MatrixNode y : rest) =
  let result = [sum $ zipWith (*) x row | row <- y]
  in VectorNode result : rest

multOp (MatrixNode x : VectorNode y : rest) =
  let result = [sum $ zipWith (*) row y | row <- x]
  in VectorNode result : rest

divOp :: Stack -> Stack
divOp (IntNode x : IntNode y : xs) = IntNode (y `div` x) : xs
divOp (FloatNode x : FloatNode y : xs) = FloatNode (y / x) : xs
divOp (IntNode x : FloatNode y : xs) = FloatNode (y / fromIntegral x) : xs
divOp (FloatNode x : IntNode y : xs) = FloatNode (fromIntegral y / x) : xs

modOp :: Stack -> Stack
modOp (IntNode x : IntNode y : xs) = IntNode (y `mod` x) : xs
modOp (FloatNode x : FloatNode y : xs) = FloatNode (y - fromIntegral (floor (y / x)) * x) : xs

expOp :: Stack -> Stack
expOp (IntNode x : IntNode y : xs) = IntNode (y ^ x) : xs
expOp (FloatNode x : FloatNode y : xs) = FloatNode (y ** x) : xs
expOp (IntNode x : FloatNode y : xs) = FloatNode (y ** fromIntegral x) : xs
expOp (FloatNode x : IntNode y : xs) = FloatNode (fromIntegral y ** x) : xs

-- create a vector node after crossing it and calulating the result array
crossOp :: Stack -> Stack
crossOp stack = case stack of
    (VectorNode [b1,b2,b3] : VectorNode [a1,a2,a3] : rest) ->
        VectorNode [ a2*b3 - a3*b2
                   , a3*b1 - a1*b3
                   , a1*b2 - a2*b1 ] : rest

castToken token
  | "'" `isPrefixOf` token =
      let strippedToken = tail token
      in if isOp strippedToken
         then QuoteNode strippedToken
         else castToken strippedToken
  | isLambda token     = LambdaNode token
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

parseVector :: String -> [Int]
parseVector s =
  case reads s of
    [(xs, "")] -> xs
    _          -> []

parseMatrix :: String -> [[Int]]
parseMatrix s =
  case reads s of
    [(xss, "")] -> xss
    _           -> []

parseStringToTokens :: String -> String -> Bool -> Bool -> Int -> [String]
parseStringToTokens [] currentStr _ _ _
    | null currentStr = []
    | otherwise = [reverse currentStr]
parseStringToTokens (currentChar:rest) currentStr insideQuotes insideBrackets depth
    | currentChar == '{' && not insideQuotes = parseStringToTokens rest (currentChar : currentStr) insideQuotes (not insideBrackets) depth
    | currentChar == '}' && not insideQuotes = parseStringToTokens rest (currentChar : currentStr) insideQuotes (not insideBrackets) depth
    | insideBrackets = parseStringToTokens rest (currentChar : currentStr) insideQuotes insideBrackets depth
    | currentChar == '[' && not insideQuotes = parseStringToTokens rest (currentChar : currentStr) insideQuotes insideBrackets (depth + 1)
    | currentChar == ']' && not insideQuotes = parseStringToTokens rest (currentChar : currentStr) insideQuotes insideBrackets (depth - 1)
    | depth > 0 = parseStringToTokens rest (currentChar : currentStr) insideQuotes insideBrackets depth
    | currentChar == '"' = parseStringToTokens rest (currentChar : currentStr) (not insideQuotes) insideBrackets depth
    | insideQuotes = parseStringToTokens rest (currentChar : currentStr) insideQuotes insideBrackets depth
    | not (isSpace currentChar) = parseStringToTokens rest (currentChar : currentStr) insideQuotes insideBrackets depth
    | not (null currentStr) = reverse currentStr : parseStringToTokens rest [] insideQuotes insideBrackets depth
    | otherwise = parseStringToTokens rest [] insideQuotes insideBrackets depth

count c = length . filter (== c)
formatList xs = "[" ++ intercalate ", " (map show xs) ++ "]"