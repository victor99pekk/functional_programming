-- Input: Two strings s and t, and values for scoreMatch, scoreMismatch, and scoreSpace.
-- Output: All optimal alignments between s and t.

-- optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]

scoreMatch :: Int
scoreMismatch :: Int
scoreSpace :: Int
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"

score :: Char -> Char -> Int
score '-' _ = scoreMismatch
score _ '-' = scoreSpace
score x y
    | x == y = scoreMatch
    | otherwise = scoreMismatch

-- 2a)
similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] (y:ys) = score '-' y + similarityScore [] ys
similarityScore (x:xs) [] = score x '-' + similarityScore xs []
similarityScore (x:xs) (y:ys) = max
        (similarityScore xs ys + score x y)
        (max
            (similarityScore xs (y:ys) + score x '-')
            (similarityScore (x:xs) ys + score '-' y)
        )

-- 2b) attachHeads takes two elements h1 and h2, and attaches them to the heads of the lists in aList.
-- aList is a list that contains two lists in each index, h1 gets added as the head of the first list
-- in each index, and h2 gets added as the head of the second list in each index.
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- 2c)
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter ((== maximumValue) . valueFcn) xs
  where
    maximumValue = maximum (map valueFcn xs)
type AlignmentType = (String,String)

-- 2d)
optAlignments :: String -> String -> [AlignmentType]
optAlignments x y = maximaBy scorefunction (createLists x y)

createLists :: String -> String -> [AlignmentType]
createLists [] [] = [("", "")]
createLists [] (y:ys) = attachHeads '-' y (createLists [] ys)
createLists (x:xs) [] = attachHeads x '-' (createLists xs [])
createLists (x:xs) (y:ys) =
    concat [ attachHeads x y (createLists xs ys)
           , attachHeads '-' y (createLists (x:xs) ys)
           , attachHeads x '-' (createLists xs (y:ys))
           ]

-- scorefunction :: AlignmentType -> Int
-- scorefunction aList = foldr (\ (x,y) acc -> (score x y) + acc) 0 aList

scorefunction :: AlignmentType -> Int
scorefunction ("", "") = 0
scorefunction ((x:xs), (y:ys)) = (score x y) + scorefunction (xs, ys)


outputOptAlignments :: String -> String -> IO ()
outputOptAlignments x y = do
    putStrLn "The best string alignments are:\n\n"
    mapM_ putStrLn $ map stringBuilder (optAlignments x y)

stringBuilder :: AlignmentType -> String
stringBuilder ("", "") = ""
stringBuilder ("", (y:ys)) = [y] ++ stringBuilder ("", ys)
stringBuilder ((x:xs), "") = [x] ++ stringBuilder (xs, "")
stringBuilder ((x:xs),(y:ys)) =  [x] ++ stringBuilder (xs, "") ++ "\n" ++ [y] ++ stringBuilder ("", ys) ++ "\n\n"


-- 3. Optimize the computing in exercise 2 through memoization

newSimilarityScore :: Eq a => [a] -> [a] -> Int
newSimilarityScore xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
       
    mcsEntry :: Int -> Int -> Int
    mcsEntry 0 j = j * scoreSpace
    mcsEntry i 0 = i * scoreSpace
    mcsEntry i j
        | x == y = max (diag + scoreMatch) (max left up)
        |otherwise = max  (diag + scoreMismatch)
                        ((max (mcsLen i (j-1)) (mcsLen (i-1) (j))) + scoreSpace)

        where
            x = xs!!(i-1)
            y = ys!!(j-1)
            up = mcsLen (i-1) j + scoreSpace
            left = mcsLen i (j-1) + scoreSpace
            diag = mcsLen (i-1) (j-1)


-- newOptAlignments :: Eq a => [a] -> [a] -> (Int, [AlignmentType])
-- newOptAlignments xs ys = mcsLen (length xs) (length ys)
--   where
--     mcsLen i j = mcsTable!!i!!j
--     mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
       
--     mcsEntry :: Int -> Int -> (Int, [AlignmentType])
--     mcsEntry 0 j = j * scoreSpace
--     mcsEntry i 0 = i * scoreSpace
--     mcsEntry i j
--         | x == y = max (diag + scoreMatch) (max left up)
--         |otherwise = maxValue

--         where
--             maxValue = max (diag + hit) (max left up)
--             x = xs!!(i-1)
--             y = ys!!(j-1)
--             up = mcsLen (i-1) j + scoreSpace
--             left = mcsLen i (j-1) + scoreSpace
--             diag = mcsLen (i-1) (j-1)
--             hit
--                 | x == y = scoreMatch
--                 |otherwise = scoreMismatch
newOptAlignments :: String -> String -> (Int, [AlignmentType])
newOptAlignments xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable !! i !! j
    mcsTable = [[mcsEntry i j | j <- [0 ..]] | i <- [0 ..]]

    mcsEntry :: Int -> Int -> (Int, [AlignmentType])
    mcsEntry 0 0 = (0, [([], [])])
    mcsEntry 0 j = (j * scoreSpace, [([], replicate j '-')])
    mcsEntry i 0 = (i * scoreSpace, [(replicate i '-', [])])
    mcsEntry i j
      | x == y = (maxScore, alignmentsWithMatch)
      | otherwise = (maxScore, alignmentsWithMismatch)
      where
        x = xs !! (i - 1)
        y = ys !! (j - 1)
        upScore = fst (mcsLen (i - 1) j) + scoreSpace
        leftScore = fst (mcsLen i (j - 1)) + scoreSpace
        diagScore = fst (mcsLen (i - 1) (j - 1))
        maxScore = maximum [diagScore + if x == y then scoreMatch else scoreMismatch, upScore, leftScore]
        alignmentsWithMatch = [(a ++ [x], b ++ [y]) | (a, b) <- snd (mcsLen (i - 1) (j - 1)), x == y]
        alignmentsWithMismatch = concatMap (\(score, aligns) -> [(a ++ [x], b ++ [y]) | (a, b) <- aligns]) $
          filter (\(score, _) -> score == maxScore) [(diagScore + if x == y then scoreMatch else scoreMismatch, snd (mcsLen (i - 1) (j - 1))), (upScore, snd (mcsLen (i - 1) j)), (leftScore, snd (mcsLen i (j - 1)))]