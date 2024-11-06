{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)

newtype Term = T {unT :: [Term]}

exec :: [Char] -> Term -> Term
exec = exec1 . drop 1 . scanl (\ case
    ('(', i) -> (, i + 1)
    (')', i) -> (, i - 1)
    (_, i) -> (, i)
    ) ('_', 0) . filter (`elem` "().01|") where

    exec1 :: [(Char, Int)] -> Term -> Term
    exec1 = \ case
        [] -> id
        s@((_, i) : _) -> case break (`elem` zip ".|" [i, i]) s of
            (s0, ('.', _) : s1) -> T . \ x -> exec2 s0 x : unT (exec1 s1 x)
            (s0, ('|', _) : s1) -> until (null . unT . exec2 s0) $ exec1 s1
            _ -> exec2 s

    exec2 :: [(Char, Int)] -> Term -> Term
    exec2 = \ case
        [] -> id
        ('0', _) : s -> exec2 s . (!! 0) . (++ [T []]) . unT
        ('1', _) : s -> exec2 s . T . drop 1 . unT
        ('(', i) : s -> case span (/= (')', i + 1)) s of
            (s0, _ : s1) -> exec2 s1 . exec1 s0
            _ -> errorWithoutStackTrace "parse error"
        _ -> errorWithoutStackTrace "parse error"

fromStr :: [Char] -> Term
fromStr = T . map (T . (`replicate` T []) . fromEnum)

toStr :: Term -> [Char]
toStr = map (toEnum . length . unT) . unT

main :: IO ()
main = do
    args <- getArgs
    [code, input] <- case args of
        [] -> sequence [getLine, getContents]
        [file] -> sequence [readFile file, getContents]
        [file0, file1] -> mapM readFile [file0, file1]
        _ -> errorWithoutStackTrace "too many arguments"
    putStr . toStr . exec code $ fromStr input