module Main where
import Syntax
import Lexer (alexScanTokens)
import Parser (logicParser)

parseLogic :: String -> Term
parseLogic s = logicParser (alexScanTokens s)

main :: IO ()
main = do
    let formula = parseLogic "¬p | ((q |~ r))"
    print formula
    let nSymbols = termFoldr (\_ -> 1) (\n -> n + 1) (\n m -> n + m + 3) (\n m -> n + m+ 3) formula
    print nSymbols
