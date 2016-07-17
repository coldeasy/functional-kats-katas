import System.Environment

data OpT t = Unary (t -> Bool) | Binary (t -> t -> Bool) | Nullary t
type Op = OpT Bool

xor :: Bool -> Bool -> Bool
xor fst snd = (fst && not snd) || (not fst && snd)

ops = [('A', Binary (&&)), ('R', Binary (||)), ('X', Binary (xor)), ('N', Unary (not))]
boolTypes = [('1', (Nullary True)), ('0', (Nullary False))]

tokens = []

parseToken :: Char -> Op
parseToken c =
    case lookup c ops of
        Just x -> x
        Nothing ->
            case lookup c boolTypes of
                Just y -> y
                _ -> error ("Invalid character " ++ (show c))

parseTokens :: String -> [Op]
parseTokens str = map parseToken $ filter (\x -> x /= ' ') str

reduceTokens :: [Op] -> [Op]
reduceTokens ((Nullary b1):(Nullary b2):(Binary op):xs) = reduceTokens ((Nullary $ op b1 b2):(reduceTokens xs))
reduceTokens ((Nullary b1):(Unary op):xs) = reduceTokens ((Nullary $ op b1):(reduceTokens xs))
reduceTokens val = val

evalTokens :: [Op] -> Bool
evalTokens [(Nullary b1)] = b1
evalTokens xs = evalTokens $ reduceTokens xs

evalRawTokenSeq :: String -> Bool
evalRawTokenSeq tokenSeq = evalTokens $ parseTokens tokenSeq

main :: IO ()
main = do
    args <- getArgs  
    case args of
        [tokenSeq] ->
            putStrLn . show . evalRawTokenSeq $ tokenSeq
        _ ->
            putStrLn "invalid input"

test :: IO ()
test = do
    case filter (\(x,y) -> y /= evalRawTokenSeq x) tests of
        [] -> putStrLn "All tests pass"
        failed -> putStrLn ("These failed: " ++ (show failed))
    where tests = [("0 1 R",                True),
                   ("0 0 R",                False),
                   ("1 0 A 1 R N N",        True),
                   ("0 0 A 0 N 0 N A R",    True),
                   ("0 1 A 0 N 1 N A R",    False),
                   ("1 0 A 1 N 0 N A R",    False),
                   ("1 1 A 1 N 1 N A R",    True),
                   ("1 1 A 1 N 1 N A X",    True),
                   ("1 1 A 0 N 0 N A X",    False)]
