module Main where
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (intercalate)
import System.Environment (getArgs)

optionParser :: Parser (String, String)
optionParser = do
    _ <- char '-'
    opt <- many1 digit
    if opt == "1" then do
        _ <- char ' '
        file1 <- many1 (letter <|> char '.')
        _ <- char ' '
        file2 <- many1 (letter <|> char '.')
        return (file1, file2)
    else fail "Invalid option"


main :: IO ()
main = do
    args <- getArgs
    let input = intercalate " " args
    print input
    let parsedArgs = parse optionParser "" input
    case parsedArgs of
        Left err -> print err
        Right (file1, file2) -> do
            treeDef <- readFile file1
            treeData <- readFile file2
            print treeDef
            print treeData
