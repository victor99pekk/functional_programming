module Chatterbot where
import Utilities
import System.Random
import Data.Char

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
{- TO BE WRITTEN -}
stateOfMind _ = return id

rulesApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
rulesApply _ = id

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
reflect = id

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


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
{- TO BE WRITTEN -}
rulesCompile _ = []


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply _ = id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wildcard (x:xs) replaces 
    | x == wildcard = replaces ++ (substitute wildcard xs replaces)
    | otherwise     = [x] ++ (substitute wildcard xs replaces)

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Just []  -- Base case: if pattern is exhausted, return empty list
match _ _ [] = Nothing -- Base case: if input string is exhausted but pattern remains, return Nothing

match wildcard (p:ps) (s:ss)
    | s /= p && wildcard /= p = Nothing
    | s == p = match wildcard ps ss
    | wildcard == p = 
      case singleWildcardMatch (p:ps) (s:ss) of
        Just _ -> singleWildcardMatch (p:ps) (s:ss)
        Nothing -> longerWildcardMatch (p:ps) (s:ss)
    where 
      -- singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
      singleWildcardMatch (_:ps) (x:xs) = 
        case match wildcard ps xs of
          Just _  -> Just [x]
          Nothing -> Nothing

      longerWildcardMatch (wc:ps) (x:xs) = 
        case match wildcard (ps) (xs) of
          Just res -> Just ([x] : res)
          Nothing -> Nothing

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

-- matchTest = match '*' testPattern testString
-- matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}


