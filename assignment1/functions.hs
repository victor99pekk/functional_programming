import Data.Maybe (listToMaybe)

substitution _ "" _ = ""
substitution wildcard (x:xs) replaces 
    | x == wildcard = replaces ++ (substitution wildcard xs replaces)
    | otherwise     = [x] ++ (substitution wildcard xs replaces)

-- match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] _ = Just []  -- Base case: if pattern is exhausted, return empty list
match _ _ [] = Nothing -- Base case: if input string is exhausted but pattern remains, return Nothing

match wildcard (p:ps) (s:ss)
    | s == p = match wildcard ps ss
    | wildcard == p = string_builder wildcard (p:ps) (s:ss) ""
    | otherwise = Nothing

-- string_builder :: Eq a => a -> [a] -> [a] -> [a] -> Maybe [a]
string_builder _ [] s string = Just (string ++ s) -- Base case: if pattern is exhausted, return the accumulated string
string_builder _ p [] string = Just (string ++ p)

string_builder wildcard (p:ps) (s:ss) string
    | p == s = iterator wildcard ps ss string -- Add character to string if they match
    | otherwise = string_builder wildcard ps ss (string ++ [s]) -- Replace wildcard in string

-- iterator :: Eq a => a -> [a] -> [a] -> [a] -> Maybe [a]
iterator _ [] _ string = Just string  -- Base case: if pattern is exhausted, return the accumulated string
iterator _ p [] string = Just (string ++ p)
iterator wildcard (p:ps) (s:ss) string
    | p == s || p == wildcard = iterator wildcard ps ss string -- Skip character if it's the same as pattern or wildcard
    | otherwise = Nothing


-- transformationApply :: Eq a => a -> ([a] -> [a]) -> 
--                        [a] -> ([a], [a]) -> Maybe [a]

-- transformationApply _ _ [] _ = Nothing -- If the input list is empty, no match is possible
-- transformationApply _ _ _ ([], _) = Nothing -- If the first pattern in the pair is empty, no match is possible

-- transformationApply wildcard match string tuples
--     | Nothing == matched = Nothing
--     | otherwise = Just (substitution wildcard (snd tuples) matched)
--     where 
--         matched = match wildcard (fst tuples) string

french = ("My name is *", "Je m'appelle *")
myname = "My name is Zacharias"
-- transformationApply :: Char -> (Char -> [a1] -> [a2] -> Maybe a3) -> [a2] -> ([a1], [Char]) -> Maybe [Char]
transformationApply _ _ [] _ = Nothing  -- If the input list is empty, no match is possible
transformationApply _ _ _ ([], _) = Nothing  -- If the first pattern in the pair is empty, no match is possible

transformationApply wildcard func string tuples
    | Just matchedList <- matched = Just (substitution wildcard (snd tuples) matchedList)  -- Handle Just case
    | otherwise = Nothing  -- Handle Nothing case
    where 
        matched = match wildcard (fst tuples) string

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]

phrase_pairs :: [PhrasePair]
phrase_pairs =
  [ (["am"],     ["are"]),
    (["was"],    ["were"]),
    (["i"],      ["you"]),
    (["i'm"],    ["you", "are"]),
    (["i'd"],    ["you", "would"]),
    (["i've"],   ["you", "have"]),
    (["i'll"],   ["you", "will"]),
    (["my"],     ["your"]),
    (["me"],     ["you"]),
    (["are"],    ["am"]),
    (["you're"], ["i", "am"]),
    (["you've"], ["i", "have"]),
    (["you'll"], ["i", "will"]),
    (["your"],   ["my"]),
    (["yours"],  ["mine"]),
    (["you"],    ["me"])
  ]


type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

-- reflect word = [y | (x,y) <- reflections, x == word]
reflect word = fmap snd . listToMaybe . filter (\(x, _) -> x == word) $ reflections

reflect word = fmap (\x -> if transformationApply '*' id x reflections == Nothing then x else transformationApply '*' id x reflections) word

words   :: String -> [String]
unwords :: [String] -> String

-- rulesApply :: [PhrasePair] -> Phrase -> Phrase

-- rulesApply [] _ = []  -- If the input list is empty, no match is possible
-- rulesApply _ [] = []  -- If the input phrase is empty, no match is possible

-- rulesApply ((pattern, replacement):pairs) phrase =
--     case transformationApply '*' id (unwords phrase) (unwords pattern, unwords replacement) of
--         Nothing -> rulesApply pairs phrase
--         Just res -> [res]

