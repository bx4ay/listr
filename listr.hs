import System.Environment ( getArgs )

newtype List = List {list :: [List]}

readL :: String -> List
readL = List . map (List . (`replicate` List []) . fromEnum)

showL :: List -> String
showL = map (toEnum . length . list) . list

data Term = Head [Term] | Tail [Term] | Cons [Term] [Term] | While [Term] [Term]

parse :: String -> [Term]
parse s = case parse1 $ concatMap (takeWhile (/= '#')) $ lines s of
    (x, "") -> x
    _ -> errorWithoutStackTrace "unmatched brackets"
    where
    parse1 :: String -> ([Term], String)
    parse1 s = let (x, s1) = parse2 [] s in case s1 of
        '.' : s2 -> let (y, s2) = parse1 s2 in ([Cons x y], s2)
        '|' : s2 -> let (y, s2) = parse1 s2 in ([While x y], s2)
        _ -> (x, s1)

    parse2 :: [Term] -> String -> ([Term], String)
    parse2 x [] = (x, [])
    parse2 x ('0' : s) = parse2 (x ++ [Head []]) s
    parse2 x ('1' : s) = parse2 (x ++ [Tail []]) s
    parse2 x ('(' : s) = case parse1 s of
            (y, ')' : s1) -> parse2 (x ++ y) s1
            _ -> errorWithoutStackTrace "unmatched brackets"
    parse2 x (c : s) | c `elem` ".)|" = (x, c : s)
    parse2 x (_ : s) = parse2 x s

exec :: [Term] -> List -> List
exec x l = foldl exec1 l x
    where
    exec1 :: List -> Term -> List
    exec1 l (Head x) = head $ (++ [List []]) $ list $ exec x l
    exec1 l (Tail x) = List $ drop 1 $ list $ exec x l
    exec1 l (Cons x y) = List (exec x l : list (exec y l))
    exec1 l (While x y) = until (null . list . exec x) (exec y) l

main :: IO ()
main = do
    args <- getArgs
    code <- case args of
        s : _ -> readFile s
        _ -> errorWithoutStackTrace "no input file"
    interact (showL . exec (parse code) . readL)
