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
    parse2 x ('0' : s) = parse2 (Head : x) s
    parse2 x ('1' : s) = parse2 (Tail : x) s
    parse2 x ('(' : s) = case parse1 s of
        (y, ')' : s1) -> parse2 (y ++ x) s1
        _ -> errorWithoutStackTrace "unmatched brackets"
    parse2 x (c : s) | c `elem` ").|" = (x, c : s)
    parse2 x (_ : s) = parse2 x s

run :: Program -> Value -> Value
run x l = foldr run1 l x
    where
    run1 :: Term -> Value -> Value
    run1 Head l = head $ value l ++ [Value []]
    run1 Tail l = Value $ drop 1 $ value l
    run1 (Cons x y) l = Value $ run x l : value (run y l)
    run1 (While x y) l = until (null . value . run x) (run y) l

main :: IO ()
main = do
    args <- getArgs
    code <- case args of
        s : _ -> readFile s
        _ -> errorWithoutStackTrace "no input file"
    interact (decode . run (parse code) . encode)
