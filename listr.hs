{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)

newtype Term = T [Term]

unT :: Term -> [Term]
unT (T x) = x

exec :: [Char] -> Term -> Term
exec = exec1 . drop 1 . scanl (\ cases
    ('(', i) c -> (c, i + 1)
    (')', i) c -> (c, i - 1)
    (_, i) c -> (c, i)
    ) ('_', 0) . filter (`elem` "().01|") where

    exec1 :: [(Char, Int)] -> Term -> Term
    exec1 = \ case
        [] -> id
        s@((_, i) : _) -> (\ case
            (s0, ('.', _) : s1) -> \ x -> T $ exec2 s0 x : unT (exec1 s1 x)
            (s0, ('|', _) : s1) -> until (null . unT . exec2 s0) $ exec1 s1
            _ -> exec2 s
            ) $ break (`elem` zip ".|" [i, i]) s

    exec2 :: [(Char, Int)] -> Term -> Term
    exec2 = \ case
        [] -> id
        ('0', _) : s -> \ case
            T (x : _) -> exec2 s x
            x -> exec2 s x
        ('1', _) : s -> exec2 s . T . drop 1 . unT
        ('(', i) : s -> (\ case
            (s0, _ : s1) -> exec2 s1 . exec1 s0
            _ -> errorWithoutStackTrace "parse error"
            ) $ span (/= (')', i + 1)) s
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
        [filename] -> sequence [readFile filename, getContents]
        [filename0, filename1] -> mapM readFile [filename0, filename1]
        _ -> errorWithoutStackTrace "too many arguments"
    putStr $ toStr $ exec code $ fromStr input