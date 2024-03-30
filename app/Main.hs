module Main where
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.List (elemIndex)

data KT k v = Leaf String | Node Int Double (KT k v) (KT k v) --deriving (Show)

instance (Show k, Show v) => Show (KT k v) where
    show (Leaf s) = "Leaf: " ++ s
    show tree = indent (0 :: Int) tree
        where
            indent a (Leaf s) = "\n" ++ spaceS a ++ "Leaf: " ++ s
            indent 0 (Node k v l r) =
                "Node: " ++ show k ++ ", " ++ show v ++ indent (2) l ++ indent (2) r
            indent a (Node k v l r) =
                "\n" ++ spaceS a ++ "Node: " ++ show k ++ ", " ++ show v ++ indent (a + 2) l ++ indent (a + 2) r
            spaceS n
                | n <= 0 = ""
                | otherwise = " " ++ spaceS (n-1)

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, x, _) = x

trd :: (a, b, c) -> c
trd (_, _, x) = x

findLeaf :: KT k v -> [Double] -> String
findLeaf (Leaf s) _ = s
findLeaf (Node {}) [] = ""
findLeaf (Node i v l r) (x:xs)
    | ((x:xs) !! i) < v = findLeaf l (x:xs)
    | ((x:xs) !! i) >= v = findLeaf r (x:xs)
    | otherwise = ""


buildTree :: [(Int, (Int, Double, String))] -> Int -> KT Int Double
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
    leading <- many ( try (char ' ')) -- stops on first non space
    return (length leading)

optionParser :: Parser (Int, String, String)
optionParser = do
    _ <- char '-'
    opt <- many1 digit
    if opt == "1" then do
        _ <- spaces
        file1 <- many1 (satisfy (/= ' ')) 
        _ <- spaces
        file2 <- many1 (satisfy (/= ' '))
        return (read opt, file1, file2)
    else if opt == "2" then do
        _ <- spaces
        file1 <- many1 (satisfy (/= ' '))
        return (read opt, file1, "")
    else fail "Invalid option"

parseNode :: Parser (Int, Double, String)
parseNode = do
    _ <- string "Node: "
    key <- many1 digit
    _ <- string ", "
    value <- many1 (digit <|> char '.' <|> char '-')
    return (read key, read value, "")

parseLeaf :: Parser (Int, Double, String)
parseLeaf = do
    _ <- string "Leaf: "
    val <- many1 anyChar
    return (-1, 0.0, val)

contentParser :: Parser (Int, (Int, Double, String))
contentParser = do
    indent <- countSpaces
    content <- parseNode <|> parseLeaf
    return (indent, content)

parseData :: Parser [Double]
parseData = do
    data' <- sepBy (many1 (digit <|> char '.' <|> char '-')) (char ',')
    return (map read data')

parseSecondData :: Parser [String]
parseSecondData = do
    sepBy (many1 (letter <|> digit <|> char '.' <|> char '-')) (char ',')

makeNonDupl :: Eq a => [a] -> [a]
makeNonDupl [] = []
makeNonDupl (x:xs) = x : makeNonDupl (filter (/= x) xs)

cnt :: Eq a => a -> [a] -> Int
cnt x = length . filter (==x)

splitData' :: [([Double], String)] -> Double -> Int -> ([([Double], String)], [([Double], String)])
splitData' [] _ _ = ([], [])
splitData' (x:xs) n i
    | ((fst x) !! i) < n = (x : fst (splitData' xs n i), snd (splitData' xs n i)) --maybe <= ?
    | otherwise = (fst (splitData' xs n i), x : snd (splitData' xs n i))


calculateGini :: [String] -> Int -> Double
calculateGini [] _ = 1.0
calculateGini y n =
    1 - sum ([(fromIntegral (cnt num y) / fromIntegral n)**2 | num <- makeNonDupl y])

splitGINI :: [([Double], String)] -> Int -> Double -> Double
splitGINI [] _ _ = 1.0
splitGINI x i n =
    let split = splitData' x n i
        gini1 = calculateGini (map snd (fst split)) (length (fst split))
        gini2 = calculateGini (map snd (snd split)) (length (snd split))
    in (fromIntegral (length (fst split)) / fromIntegral (length x)) * gini1 + (fromIntegral (length (snd split)) / fromIntegral (length x)) * gini2

checkForLeaf :: [([Double], String)] -> Bool
checkForLeaf x = 
    length (makeNonDupl (map snd x)) == 1

-- get the best index to split by
getBestIndex :: [[Double]] -> (Double, Int, Int) -> Int -> (Int, Int)
getBestIndex [] (_, index, innerIndex) _ = (index, innerIndex)
getBestIndex (gini:xs) (minGini, index, innerIndex) n
    | minimum gini < minGini = getBestIndex xs (minimum gini, n, fromJust (elemIndex (minimum gini) gini)) (n+1)
    | otherwise = getBestIndex xs (minGini, index, innerIndex) (n+1)

-- build the tree
-- [([Double], String)] - parsed data
-- [Int] - indicating depth
-- Int - tracking the indent
createProtoTree :: [([Double], String)] -> Int -> [(Int, (Int, Double, String))]
createProtoTree [] _ = []
createProtoTree x n
    | checkForLeaf x = [(n, (-1, 0.0, snd (head x)))]
    | otherwise = 
        let indeces = getBestIndex [map (splitGINI x l . (!! l) . fst) x | l <- [0..(length (fst (head x)) - 1)]] (1,0,0) 0
            a = splitData' x (fst (x !! snd indeces) !! (fst indeces)) (fst indeces)
        in (n, (fst indeces, fst (x !! snd indeces) !! (fst indeces), "" :: String)) : createProtoTree (fst a) (n+2) ++ createProtoTree (snd a) (n+2)

main :: IO ()
main = do
    args <- getArgs
    case (parse optionParser "" (unwords args)) of
        Left err -> print err
        Right (opt, file1, file2) -> do
            case opt of
                1 -> do
                    treeDef <- readFile file1
                    treeData <- readFile file2
                    let tree = buildTree [x | (Right x) <- map (parse contentParser "") (lines treeDef)] 0
                    mapM_ (putStrLn . findLeaf tree) [x | (Right x) <- map (parse parseData "") (lines treeData)]
                2 -> do
                    treeData <- readFile file1
                    let values = [x | (Right x) <- map (parse parseSecondData "") (lines treeData)]
                    let floats = map (map read . init) values :: [[Double]] -- needed
                    print $ buildTree (createProtoTree (zip floats (map last values)) 0) 0
                _ -> print "Invalid option"
