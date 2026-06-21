import System.Environment ( getArgs )

newtype Value = V { unV :: [Value] }

encode :: String -> Value
encode = V . map (V . (`replicate` V []) . fromEnum)

decode :: Value -> String
decode = map (toEnum . length . unV) . unV

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
run = flip $ foldr run1
    where
    run1 :: Term -> Value -> Value
    run1 Head = head . (++ [V []]) . unV
    run1 Tail = V . drop 1 . unV
    run1 (Cons x y) = \ v -> V $ run x v : unV (run y v)
    run1 (While x y) = until (null . unV . run x) $ run y

main :: IO ()
main = do
    args <- getArgs
    code <- case args of
        s : _ -> readFile s
        _ -> errorWithoutStackTrace "no input file"
    interact (decode . run (parse code) . encode)
