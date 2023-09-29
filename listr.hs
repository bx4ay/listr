{-# LANGUAGE LambdaCase #-}

import Data.Bifunctor (first)
import System.Environment (getArgs)

newtype Term = T [Term]

unT :: Term -> [Term]
unT (T x) = x

exec :: [Char] -> Term -> Term
exec = exec1 . tail . scanl (\ cases
    ('(', i) c -> (c, i + 1)
    (')', i) c -> (c, i - 1)
    (_, i) c -> (c, i)
    ) (' ', 0) . filter (`elem` "().01|") where
    exec1 = \ case
        [] -> id
        s -> (\ case
            (s0, _ : s1) -> head . dropWhile (not . null . unT . exec2 s0) . iterate (exec1 s1)
            _ -> exec2 s
            ) $ span (/= ('|', snd $ head s)) s
    exec2 = \ case
        [] -> id
        s -> (\ case
            (s0, _ : s1) -> \ x -> T $ exec3 s0 x : unT (exec2 s1 x)
            _ -> exec3 s
            ) $ span (/= ('.', snd $ head s)) s
    exec3 = \ case
        [] -> id
        ('0', _) : s -> \ case
            T (x : _) -> exec3 s x
            x -> x
        ('1', _) : s -> \ case
            T (_ : x) -> exec3 s $ T x
            x -> x
        ('(', i) : s -> (\ case
            (s0, _ : s1) -> exec3 s1 . exec1 s0
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