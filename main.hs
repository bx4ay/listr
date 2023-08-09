{-# LANGUAGE LambdaCase #-}
import Data.Bifunctor (first)
import System.Environment (getArgs)
newtype Term = T [Term]
unT (T x) = x
exec = exec' . tail . scanl (\ cases
    ('(', i) c -> (c, i + 1)
    (')', i) c -> (c, i - 1)
    ('[', i) c -> (c, i + 1)
    (']', i) c -> (c, i - 1)
    (_, i) c -> (c, i)
    ) (' ', 0) . filter (`elem` "().01[]|") where
    exec' = \ case
        [] -> id
        t -> (\ case
            (t0, _ : t1) -> \ x -> T $ exec'' t0 x : unT (exec' t1 x)
            _ -> exec'' t
            ) $ span (/= ('.', snd $ head t)) t
    exec'' = \ case
        [] -> id
        ('0', _) : t -> \ case
            T (x0 : _) -> exec' t x0
            x -> x
        ('1', _) : t -> \ case
            T (_ : x1) -> exec' t $ T x1
            x -> x
        ('(', i) : t -> (\ case
            (t0, _ : t1) -> exec'' t1 . exec' t0
            _ -> errorWithoutStackTrace "parse error"
            ) $ span (/= (')', i + 1)) t
        ('[', i) : t -> (\ case
            ((t0, _ : t1), _ : t2) -> exec'' t2 . head . dropWhile (not . null . unT . exec' t0) . iterate (exec' t1)
            _ -> errorWithoutStackTrace "parse error"
            ) $ first (span (/= ('|', i + 1))) $ span (/= (']', i + 1)) t
        _ -> errorWithoutStackTrace "parse error"
fromStr = T . map (T . (`replicate` T []) . fromEnum)
toStr = map (toEnum . length . unT) . unT
main = do
    args <- getArgs
    [code, input] <- case args of
        [] -> sequence [getLine, getContents]
        [filename] -> sequence [readFile filename, getContents]
        [filename0, filename1] -> mapM readFile [filename0, filename1]
        _ -> errorWithoutStackTrace "too many arguments"
    putStr $ toStr $ exec code $ fromStr input