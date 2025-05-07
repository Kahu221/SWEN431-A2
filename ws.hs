module Main where
import System.Environment
import Control.Monad
import Text.Parsec.Expr (Operator)
import Data.Bits (xor, shiftL, shiftR, complement)
import Data.Char (isSpace, isDigit, toLower)
import Data.Fixed (mod')
import Data.List (transpose, intercalate)
import Debug.Trace (trace)

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

type Stack = [Value]

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  let input = head args
      inputFile = takeFileName input
      outputFile = "output-" ++ drop 6 inputFile
      result = process contents
  putStrLn $ show (reverse result)
  writeFile outputFile (unlines (map show (reverse result)))

process input = evalTokens [] (tokenize input)

evalTokens :: Stack -> [String] -> Stack
evalTokens stack [] = stack
evalTokens stack (token : tokens) =
    let newStack = evalToken stack token
    in evalTokens newStack tokens

evalToken :: Stack -> String -> Stack
evalToken stack token =
    case token of
        -- "+"  -> addOp (+) (+) (++) (addVec) (addMat) stack
        "+"  -> addOp stack
        "*"  -> multOp stack
        "-"  -> binNumOp (-) (-) stack
        "/"  -> binNumOp div (/) stack
        "%"  -> binNumOp mod (mod') stack
        "**" -> binNumOp (^) (**) stack
        "x"  -> crossOp stack

        "DROP"  -> dropOp stack
        "DUP"   -> dupOp stack
        "SWAP"  -> swapOp stack
        "ROT"  -> rotOp stack
        "ROLL" -> rollOp stack
        "ROLLD"-> rolldOp stack
        "IFELSE" -> ifElseOp stack
        "TRANSP" -> transpOp stack
        "EVAL" -> evalOp stack

        "==" -> cmpOp (==) stack
        "!=" -> cmpOp (/=) stack
        ">"  -> cmpOp (>)  stack
        "<"  -> cmpOp (<)  stack
        ">=" -> cmpOp (>=) stack
        "<=" -> cmpOp (<=) stack
        "<=>" -> comparison stack

        "&"  -> boolOp (&&) stack
        "|"  -> boolOp (||) stack
        "^"  -> xorOp stack
        "<<" -> binIntOp shiftL stack
        ">>" -> binIntOp shiftR stack
        "!"  -> unaryBoolOp not stack
        "~"  -> unaryIntOp complement stack

        _    -> createValue token : stack -- otherwise is a number or string

-- Helper function to convert strings to appropriate Value type
createValue s
  | head s == '"' && last s == '"' = VStr (init (tail s))
  | count '[' s == 1 && head s == '[' && last s == ']' = VVector (read s)
  | count '[' s > 1 && head s == '[' && last s == ']' = VMatrix (read s)
  | head s == '{' && last s == '}' = VLambda s
  | s == "true"  = VBool True
  | s == "false" = VBool False
  | '.' `elem` s && all (`elem` "-.0123456789") s = VFloat (read s)
  | all (`elem` "-0123456789") s = VInt (read s)
  | head s == '\'' = createValue (tail s) -- single quote
  | otherwise =  VQuoting s   -- quoting

number :: Value -> Value
number (VInt x) = VFloat (fromIntegral x)
number x = x

binIntOp f (VInt x : VInt y : rest) = VInt (f y x) : rest

binNumOp :: (Int -> Int -> Int)
      -> (Float -> Float -> Float)
      -> [Value] -> [Value]
binNumOp intF floatF (x : y : rest) = case (x, y) of
  (VInt a, VInt b)       -> VInt   (intF b a)   : rest
  (VFloat a, VFloat b)   -> VFloat (floatF b a) : rest
  (VInt a, VFloat b)     -> VFloat (floatF b (fromIntegral a)) : rest
  (VFloat a, VInt b)     -> VFloat (floatF (fromIntegral b) a) : rest

addOp (x : y : rest) = case (x, y) of
      (VInt a, VInt b)       -> VInt (b + a) : rest
      (VFloat a, VFloat b)   -> VFloat (b + a) : rest
      (VInt a, VFloat b)     -> VFloat (b + (fromIntegral a)) : rest
      (VFloat a, VInt b)     -> VFloat ((fromIntegral b) + a) : rest
      (VStr a, VStr b)       -> VStr (b ++ a) : rest
      (VVector a, VVector b) -> VVector (addVec b a) : rest
      (VMatrix a, VMatrix b) -> VMatrix (addMat b a) : rest

multOp (x : y : rest) = case (x, y) of
      (VInt a, VInt b)       -> VInt (b * a) : rest
      (VFloat a, VFloat b)   -> VFloat (b * a) : rest
      (VInt a, VFloat b)     -> VFloat (b * (fromIntegral a)) : rest
      (VFloat a, VInt b)     -> VFloat ((fromIntegral b) * a) : rest
      (VInt a, VStr b)       -> VStr (concat (replicate a b)) : rest
      (VVector a, VVector b) -> VInt (dot a b) : rest
      (VVector a, VMatrix b) -> VVector (multMatVec a b) : rest
      (VMatrix a, VVector b) -> VVector (multMatVec b a) : rest
      (VMatrix a, VMatrix b) -> VMatrix (multMat b a) : rest

-- matrix and vector operations
dot v1 v2 = sum (zipWith (*) v1 v2)
addVec = zipWith (+)
addMat = zipWith addVec
multMat a b =
  let bt = transpose b
  in [ [ dot row col | col <- bt ] | row <- a ]
multMatVec v m = map (`dot` v) m

cross [a1,a2,a3] [b1,b2,b3] =
  [ a2*b3 - a3*b2
  , a3*b1 - a1*b3
  , a1*b2 - a2*b1 ]
crossOp (VVector x:VVector y:xs) = VVector(cross y x):xs
transpOp (VMatrix x:xs) = VMatrix(transpose x):xs

dropOp (x:xs) = xs
dupOp (x:xs)  = x:x:xs
swapOp (x:y:xs) = y:x:xs
rotOp (x:y:z:rest) = z:x:y:rest

cmpOp :: (forall a. Ord a => a -> a -> Bool) -> Stack -> Stack
cmpOp f (VInt a : VInt b : rest) = VBool (f b a) : rest
cmpOp f (VFloat a : VFloat b : rest) = VBool (f b a) : rest
cmpOp f (VStr a : VStr b : rest) = VBool (f b a) : rest

boolOp :: (Bool -> Bool -> Bool) -> Stack -> Stack
boolOp f (VBool x : VBool y : rest) = VBool (f y x) : rest

rollOp (VInt n : rest) =
  let (top, bottom) = splitAt n rest
      rotatedTop = last top : init top
  in rotatedTop ++ bottom

rolldOp (VInt n : rest) =
  let (x:xs, bottom) = splitAt n rest
  in xs ++ [x] ++ bottom

comparison (x : y : rest) =
  let sx = show x
      sy = show y
  in case compare sy sx of
       LT -> VInt (-1) : rest
       EQ -> VInt 0    : rest
       GT -> VInt 1    : rest

xorOp (VBool x : VBool y : rest) =
  VBool (x /= y) : rest
xorOp (VInt x : VInt y : rest) =
  VInt (x `xor` y) : rest

ifElseOp (VBool cond : falseVal : trueVal : rest) =
  (if cond then trueVal else falseVal) : rest

unaryBoolOp f (VBool x : rest) = VBool (f x) : rest
unaryIntOp f (VInt x : rest) = VInt (f x) : rest

-- evalOp (VLambda lambda : rest) =
--     trace "Pattern match succeeded!"
--   let body = (init (tail lambda))
--   in evalToken rest (show body)
--   --in evalToken rest (show x)

evalOp (x : rest) = evalToken rest (show x)

tokenize:: [Char] -> String -> Bool -> Bool -> Int -> [String]
tokenize [] s   _
        | null s = []
        |otherwise =[reverse s]
tokenize (o:ox) s quoted brackted count 
        | '{' == o && not quoted = tokenize ox (o : s) quoted (not brackted) count

        | '}' == o && not quoted = tokenize ox (o : s) quoted (not brackted) count
        | brackted = tokenize ox (o : s) quoted brackted count
        -- if it's a open square bracket incriment it 
        | '[' == o && not quoted = tokenize ox (o : s) quoted brackted (count + 1)
        -- ig it's a closed square bracker deincriment it
        | ']' == o && not quoted = tokenize ox (o : s) quoted brackted (count - 1)
        -- if it's going to be a string keep creating a token until not quated
        | count > 0 = tokenize ox (o : s) quoted brackted count
        | '"' == o = tokenize ox (o : s) (not quoted) brackted count
        -- If we're still in quoated just keep creating the token
        | quoted = tokenize ox (o : s) quoted brackted count
        -- if it's not a space keep creating the token
        | not (isSpace o) = tokenize ox (o : s) quoted brackted count
        | not (null s) = reverse s : tokenize ox [] quoted brackted count
        -- if it's a space skip it 
        | otherwise = tokenize ox [] quoted brackted count

-- more helper functions
count c = length . filter (== c)
formatList xs = "[" ++ intercalate ", " (map show xs) ++ "]"
