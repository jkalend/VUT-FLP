module Main where
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (intercalate)
import Data.Char (isSpace)
import System.Environment (getArgs)

-- option A: have tree as an array of tuples
-- option B: have tree as a proper data type

data KT k v = EmptyKT | Leaf String | NodeKT k v (KT k v) (KT k v) deriving (Show)

treeAddKT :: (Ord k) => (k, v) -> KT k v -> Either String (KT k v)
treeAddKT (k, v) EmptyKT = Right (NodeKT k v EmptyKT EmptyKT)
treeAddKT (k, v) (Leaf _) = Left "Cannot add to leaf"
treeAddKT (k, v) (NodeKT k' v' l r)
    | k == k' = Left "Key already exists"
    | k < k' = case treeAddKT (k, v) l of
        Left err -> Left err
        Right l' -> Right (NodeKT k' v' l' r)
    | k > k' = case treeAddKT (k, v) r of
        Left err -> Left err
        Right r' -> Right (NodeKT k' v' l r')

test :: IO ()
test = do
    let tree = treeAddLeafKT "a" $ treeAddKT (1, 5.5) EmptyKT
    print tree

treeAddLeafKT :: (Ord k) => (k, String) -> KT k v -> Either String (KT k v)
treeAddLeafKT (k, v) EmptyKT = Right (Leaf v)
treeAddLeafKT (k, v) (Leaf _) = Left "Cannot add to leaf"
treeAddLeafKT (k, v) (NodeKT k' v' l r)
    | k == k' = Left "Key already exists"
    | k < k' = case treeAddLeafKT (k, v) l of
        Left err -> Left err
        Right l' -> Right (NodeKT k' v' l' r)
    | k > k' = case treeAddLeafKT (k, v) r of
        Left err -> Left err
        Right r' -> Right (NodeKT k' v' l r')
    

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
        file1 <- many1 (letter <|> char '.')
        _ <- char ' '
        file2 <- many1 (letter <|> char '.')
        return (file1, file2)
    else fail "Invalid option"


main :: IO ()
main = do
    args <- getArgs
    let input = intercalate " " args
    -- print input
    let parsedArgs = parse optionParser "" input
    case parsedArgs of
        Left err -> print err
        Right (file1, file2) -> do
            treeDef <- readFile file1
            let treeLines = lines treeDef
            treeData <- readFile file2
            let dataLines = lines treeData
            -- mapM_ putStrLn treeLines
            let ddd = [x | (Right x) <- map (parse countSpaces "") treeLines]
            -- let ddd = zip [1..] treeLines
            print ddd
            print treeData
