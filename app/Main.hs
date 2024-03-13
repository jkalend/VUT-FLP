module Main where
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Environment (getArgs)

data KT k v = Empt | Leaf String | Node Int Float (KT k v) (KT k v) deriving (Show)

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, x, _) = x

trd :: (a, b, c) -> c
trd (_, _, x) = x

findLeaf :: KT k v -> [Float] -> String
findLeaf (Leaf s) _ = s
findLeaf Empt _ = ""
findLeaf (Node {}) [] = ""
findLeaf (Node _ v l r) (x:xs)
    | x < v = findLeaf l xs
    | x > v = findLeaf r xs
    | otherwise = ""

buildTree :: [(Int, (Int, Float, String))] -> Int -> KT Int Float
buildTree [] _  = Leaf ""
buildTree (x:xs) i
    | fst x == i =
        if fst' (snd x) == -1 then Leaf (trd (snd x))
        else if fst (head xs) == i+2 then Node (fst' (snd x)) (snd' (snd x)) (buildTree xs (i+2)) (buildTree (tail xs) (i+2))
        else Node (fst' (snd x)) (snd' (snd x)) (buildTree xs (i+2)) (buildTree xs (i+2))
    | fst x > i = buildTree xs i
    | otherwise = Leaf ""

countSpaces :: Parser Int
countSpaces = do
    leading <- many ( try (char ' '))
    return (length leading)

optionParser :: Parser (String, String)
optionParser = do
    _ <- char '-'
    opt <- many1 digit
    if opt == "1" then do
        _ <- char ' '
        file1 <- many1 (letter <|> char '.' <|> digit)
        _ <- char ' '
        file2 <- many1 (letter <|> char '.' <|> digit)
        return (file1, file2)
    else fail "Invalid option"

parseNode :: Parser (Int, Float, String)
parseNode = do
    _ <- string "Node: "
    key <- many1 digit
    _ <- string ", "
    value <- many1 (digit <|> char '.')
    return (read key, read value, "")

parseLeaf :: Parser (Int, Float, String)
parseLeaf = do
    _ <- string "Leaf: "
    val <- many1 (letter <|> digit)
    return (-1, 0.0, val)

contentParser :: Parser (Int, (Int, Float, String))
contentParser = do
    indent <- countSpaces
    content <- parseNode <|> parseLeaf
    return (indent, content)

parseData :: Parser [Float]
parseData = do
    data' <- sepBy (many1 (digit <|> char '.')) (char ',')
    return (map read data')

main :: IO ()
main = do
    args <- getArgs
    let input = unwords args
    let parsedArgs = parse optionParser "" input
    case parsedArgs of
        Left err -> print err
        Right (file1, file2) -> do
            treeDef <- readFile file1
            treeData <- readFile file2
            let tree = buildTree [x | (Right x) <- map (parse contentParser "") (lines treeDef)] 0
            -- print t
            let values = [x | (Right x) <- map (parse parseData "") (lines treeData)]
            -- print b
            mapM_ (putStrLn . findLeaf tree) values
