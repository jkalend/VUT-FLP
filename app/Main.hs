module Main where
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Environment (getArgs)
import Data.List (nubBy)

data KT k v = Empt | Leaf String | Node Int Float (KT k v) (KT k v)

instance (Show k, Show v) => Show (KT k v) where
    show Empt = "Empty"
    show (Leaf s) = "Leaf: " ++ s
    show tree = indent (0 :: Int) tree
        where
            indent a Empt = spaceS a ++ "Empty\n"
            indent a (Leaf s) = spaceS a ++ "Leaf: " ++ s ++ "\n"
            indent a (Node k v l r) =
                spaceS a ++ "Node: " ++ show k ++ ", " ++ show v ++ "\n" ++ indent (a + 2) l ++ indent (a + 2) r
            spaceS n
                | n <= 0 = ""
                | otherwise = " " ++ spaceS (n-1)

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
    leading <- many ( try (char ' ')) -- stops on first non space
    return (length leading)

optionParser :: Parser (Int, String, String)
optionParser = do
    _ <- char '-'
    opt <- many1 digit
    if opt == "1" then do
        _ <- char ' '
        file1 <- many1 (letter <|> char '.' <|> digit)
        _ <- char ' '
        file2 <- many1 (letter <|> char '.' <|> digit)
        return (read opt, file1, file2)
    else if opt == "2" then do
        _ <- char ' '
        file1 <- many1 (letter <|> char '.' <|> digit)
        return (read opt, file1, "")
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

parseSecondData :: Parser [String]
parseSecondData = do
    sepBy (many1 (letter <|> digit <|> char '.')) (char ',')

getNthElements :: Int -> [Float] -> Float
getNthElements _ [] = 0
getNthElements n x
    | n < length x = x !! n
    | otherwise = 0

makeNonDupl :: Eq a => [a] -> [a]
makeNonDupl [] = []
makeNonDupl (x:xs) = x : makeNonDupl (filter (/= x) xs)

cnt :: Eq a => a -> [a] -> Int
cnt x = length . filter (==x)

splitData :: [([Float], String)] -> Float -> ([([Float], String)], [([Float], String)])
splitData [] _ = ([], [])
splitData (x:xs) n
    | head (fst x) < n = (x : fst (splitData xs n), snd (splitData xs n)) --maybe <= ?
    | otherwise = (fst (splitData xs n), x : snd (splitData xs n))



--makeSecondTreeString :: [([Float], String)] -> String
--makeSecondTreeString [] = "" --take the next float in the array of floats and split according to it
 -- [([2.4,1.3],"TridaA"),([6.1,0.3],"TridaB"),([6.3,4.4],"TridaC"),([2.9,4.4],"TridaA"),([3.1,2.9],"TridaB")]
 -- will check split by 2.4 and see 6.1 go right and so on
 -- then it will check 6.1 and so on until it pick the one with the lowest gini
    -- then it will split the array of floats by the value and repeat the process
    -- until it reaches the end of the array

calculateGini :: [String] -> Int -> Float
calculateGini [] _ = 1.0
calculateGini y n =
    --1 - ((up / down)**2) - calculateGini xs y n
    1 - sum ([(fromIntegral (cnt num y) / fromIntegral n)**2 | num <- makeNonDupl y])

splitGINI :: [([Float], String)] -> Float -> Float
splitGINI [] _ = 1.0
splitGINI x n =
    let a = splitData x n
        b = calculateGini (map snd (fst a)) (length (fst a))
        c = calculateGini (map snd (snd a)) (length (snd a))
    in (fromIntegral (length (fst a)) / fromIntegral (length x)) * b + (fromIntegral (length (snd a)) / fromIntegral (length x)) * c


-- split the data by the value and calculate the gini index
-- repeat the process until the gini index is the lowest
getBestGINI :: [([Float], String)] -> Int -> ([Float], String)
getBestGINI x n =
    let a = map (splitGINI x . (!! n) . fst) x
        b = minimum a
    in x !! length (takeWhile (/= b) a)

checkForLeaf :: [([Float], String)] -> Bool
checkForLeaf x = 
    length (makeNonDupl (map snd x)) == 1
        

-- build the tree
-- [([Float], String)] - parsed data
-- [Int] - indicating depth
-- Int - tracking the indent
gg :: [([Float], String)] -> [Int] -> Int -> [(Int, (Int, Float, String))]
gg (x:xs) [] n = (n, (-1, 0.0, snd x)) : gg xs [] (n+2)
gg [] [] _ = []
gg x (y:ys) n
    | checkForLeaf x = [(n, (-1, 0.0, snd (head x)))]
    | otherwise = 
        let splt = getBestGINI x y
            a = splitData x (head (fst splt))
        in (n, (0, fst splt !! y, "" :: String)) : gg (fst a) ys (n+2) ++ gg (snd a) ys (n+2)

fixIndex:: [(Int, (Int, Float, String))] -> Int -> [(Int, (Int, Float, String))]
fixIndex [] _ = []
fixIndex (x:xs) n
    | fst' (snd x) == 0 = (fst x, (n, snd' (snd x), trd (snd x))) : fixIndex xs (n+1)
    | otherwise = x : fixIndex xs n

-- TODO remove
-- debugSplit :: [([Float], String)] -> [Int] -> [([Float], String)]
-- debugSplit _ [] = []
-- debugSplit x (y:ys) 
--     | checkForLeaf x = [([0.0], snd (head x))]
--     | otherwise = 
--         let a = splitData x (head (fst (getBestGINI x y)))
--         in getBestGINI x y : debugSplit (fst a) ys ++ debugSplit (snd a) ys
--     -- let a = splitData x (head (fst (getBestGINI x y)))
--     -- -- in fst a
--     -- in 
--     --     if (fst a) getBestGINI x y : debugSplit (fst a) ys ++ debugSplit (snd a) ys

main :: IO ()
main = do
    args <- getArgs
    let input = unwords args
    let parsedArgs = parse optionParser "" input
    case parsedArgs of
        Left err -> print err
        Right (opt, file1, file2) -> do
            case opt of
                1 -> do
                    treeDef <- readFile file1
                    treeData <- readFile file2
                    let tree = buildTree [x | (Right x) <- map (parse contentParser "") (lines treeDef)] 0
                    let values = [x | (Right x) <- map (parse parseData "") (lines treeData)]
                    mapM_ (putStrLn . findLeaf tree) values
                    --print values
                2 -> do
                    treeData <- readFile file1
                    let values = [x | (Right x) <- map (parse parseSecondData "") (lines treeData)]
                    let floats = map (map read . init) values :: [[Float]] -- needed 
                    let c = zip floats (map last values)
                    print c
                    -- let d = makeNonDupl (map last values)
                    -- print d
                    -- let overallGini = calculateGini (map last values) (length values)
                    let f = getBestGINI c 0 -- get the best split for the current data and first value
                    let split1 = splitData c (head (fst (getBestGINI c 0))) -- split the data by the value from f

                    -- let d = checkForLeaf (fst split1)

                    let test = gg c [0..(length (fst (head c)) - 1)] 0
                    -- let t2 = makeNonDupl test
                    let t3 = nubBy (\x y -> snd x == snd y && fst x < fst y) test
                    -- let dbg = debugSplit c [0..(length (fst (head c)) - 1)]

                    print f
                    print split1
                    -- print t2
                    print t3
                    -- print $ fixIndex t3 0
                    print $ buildTree (fixIndex t3 0) 0
                    -- print test
                    -- print dbg
                    -- print overallGini
                _ -> print "Invalid option"
