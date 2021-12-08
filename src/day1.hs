countIncrease :: (Ord a) => [a] -> Int -> Int
countIncrease [] acc = acc
countIncrease (_:[]) acc = acc
countIncrease (a:b:cs) acc
              | b > a = countIncrease (b:cs) acc+1
              | otherwise = countIncrease (b:cs) acc

convertToInt :: [String] -> [Int]
convertToInt = map (read :: String->Int)

process filename = do
    content <- readFile filename
    let stringData = lines content
    let intData = convertToInt stringData
    let answer = countIncrease intData 0
    putStrLn $ show answer

extractWindow :: (Num a) => [a] -> Maybe a
extractWindow []        = Nothing
extractWindow (_:[])    = Nothing
extractWindow (_:_:[])  = Nothing
extractWindow (a:b:c:_) = Just (a+b+c)

extractWindows :: [Int] -> [Maybe Int] -> [Maybe Int]
extractWindows [] acc = acc
extractWindows xs acc = extractWindows rest newacc
    where rest = tail xs
          newacc = (extractWindow xs) : acc

findWindowedTotals :: [Int] -> Maybe Int -> [Int]
findWindowedTotals acc Nothing = acc
findWindowedTotals acc (Just value) = value : acc

processWindow filename = do
    content <- readFile filename
    let stringData = lines content
    let intData = convertToInt stringData
    let maybeWindowedData = extractWindows intData []
    let windowedData = foldl findWindowedTotals [] maybeWindowedData
    let answer = countIncrease windowedData 0
    putStrLn $ show answer
