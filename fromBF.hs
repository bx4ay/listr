{-# LANGUAGE LambdaCase #-}
import System.Environment (getArgs)
fromBF s = "(.00)(1.1.1.0.1)" ++ concatMap (\ case
    '+' -> "(0(0.).1)"
    ',' -> "(11100.10.110.11101.1111)"
    '-' -> "(01.1)"
    '.' -> "(0.10.110.1110.(0.11110).11111)"
    '<' -> "(100.101.(0.110).111)"
    '>' -> "(1100.(0.10).1101.111)"
    '[' -> "(0|"
    ']' -> ")"
    _ -> ""
    ) s ++ "1111(0.1.1)(0|01.(00.10).11)10"
main = do
    args <- getArgs
    s <- case args of
        [] -> getLine
        [filename] -> readFile filename
        _ -> errorWithoutStackTrace "too many arguments"
    putStrLn $ fromBF s