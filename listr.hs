import System.Environment ( getArgs )

newtype Value = Value { value :: [Value] }

encode :: String -> Value
encode = Value . map (Value . (`replicate` Value []) . fromEnum)

decode :: Value -> String
decode = map (toEnum . length . value) . value

type Program = [Term]

data Term = Head | Tail | Cons Program Program | While Program Program

parse :: String -> Program
parse s = case parse1 $ concatMap (takeWhile (/= '#')) $ lines s of
    (x, []) -> x
    _ -> errorWithoutStackTrace "unmatched brackets"
    where
    parse1 :: String -> (Program, String)
    parse1 s = case parse2 [] s of
        (x, '.' : s1) -> let (y, s2) = parse1 s1 in ([Cons x y], s2)
        (x, '|' : s1) -> let (y, s2) = parse1 s1 in ([While x y], s2)
        (x, s1) -> (x, s1)

    parse2 :: Program -> String -> (Program, String)
    parse2 x [] = (x, [])
    parse2 x ('0' : s) = parse2 (x ++ [Head]) s
    parse2 x ('1' : s) = parse2 (x ++ [Tail]) s
    parse2 x ('(' : s) = case parse1 s of
        (y, ')' : s1) -> parse2 (x ++ y) s1
        _ -> errorWithoutStackTrace "unmatched brackets"
    parse2 x (c : s) | c `elem` ").|" = (x, c : s)
    parse2 x (_ : s) = parse2 x s

run :: Program -> Value -> Value
run x l = foldl run1 l x
    where
    run1 :: Value -> Term -> Value
    run1 l Head = head $ value l ++ [Value []]
    run1 l Tail = Value $ drop 1 $ value l
    run1 l (Cons x y) = Value $ run x l : value (run y l)
    run1 l (While x y) = until (null . value . run x) (run y) l

main :: IO ()
main = do
    args <- getArgs
    code <- case args of
        s : _ -> readFile s
        _ -> errorWithoutStackTrace "no input file"
    interact (decode . run (parse code) . encode)
