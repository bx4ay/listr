{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)

newtype Term = T {unT :: [Term]}

exec :: String -> Term -> Term
exec = exec1 . drop 1 . scanl (\ case
    ('(', i) -> (, i + 1)
    (')', i) -> (, i - 1)
    (_, i) -> (, i)
    ) (' ', 0) . concatMap (filter (`elem` "().01|") . takeWhile (/= '#')) . lines where

    exec1 :: [(Char, Int)] -> Term -> Term
    exec1 = \ case
        [] -> id
        s@((_, i) : _) -> case break (`elem` map (, i) ".|") s of
            (s0, ('.', _) : s1) -> \ x -> T $ exec2 s0 x : unT (exec1 s1 x)
            (s0, ('|', _) : s1) -> until (null . unT . exec2 s0) $ exec1 s1
            _ -> exec2 s

    exec2 :: [(Char, Int)] -> Term -> Term
    exec2 = \ case
        [] -> id
        ('0', _) : s -> exec2 s . (!! 0) . (++ [T []]) . unT
        ('1', _) : s -> exec2 s . T . drop 1 . unT
        ('(', i) : s -> case break (== (')', i + 1)) s of
            (s0, _ : s1) -> exec2 s1 . exec1 s0
            _ -> errorWithoutStackTrace "parse error"
        _ -> errorWithoutStackTrace "parse error"

fromStr :: String -> Term
fromStr = T . map (T . (`replicate` T []) . fromEnum)

toStr :: Term -> String
toStr = map (toEnum . length . unT) . unT

main :: IO ()
main = do
    args <- getArgs
    code <- case args of
        path : _ -> readFile path
        _ -> errorWithoutStackTrace "no input files"
    interact $ toStr . exec code . fromStr
