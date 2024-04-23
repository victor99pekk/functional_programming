-- Victor Pekkari, Brasse Wiklund
module Chatterbot where

import Data.Char
import Data.Maybe
import System.Random
import Utilities

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
--t
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind botbrain =
  fmap (\r -> rulesApply $ (map . map2) (id, pick r) botbrain) (randomIO :: IO Float)

reflect :: Phrase -> Phrase
reflect = map $ \word -> fromMaybe word (lookup word reflections)

reflect_help :: String -> [(String, String)] -> String
reflect_help word [] = word
reflect_help word (tuple:tuples) 
  | word == (fst tuple) = snd tuple
  | otherwise = reflect_help word tuples

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

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map (\(string, strings) -> (words $ map toLower string, map words strings))

-- ---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

-- --------------------------------------


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
reductionsApply = fix . try . transformationsApply "*" id


-- -------------------------------------------------------
-- -- Match and substitute
-- --------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wildcard (x:xs) replaces
    | x == wildcard = replaces ++ (substitute wildcard xs replaces)
    | otherwise     = [x] ++ (substitute wildcard xs replaces)

match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ _ [] = Nothing
match _ [] _ = Nothing

match wildcard (p:ps) (s:ss)
    | s /= p && wildcard /= p = Nothing
    | s == p = match wildcard ps ss
    | wildcard == p = 
      case singleWildcardMatch (p:ps) (s:ss) of
        Just res -> Just ([s])
        Nothing -> longerWildcardMatch (p:ps) (s:ss)
    where
      singleWildcardMatch [] _ = Just []
      singleWildcardMatch (p:ps) (s:ss) = mmap (s :) (match wildcard ps ss)

      longerWildcardMatch [] s = Nothing
      longerWildcardMatch p [] = Nothing
      longerWildcardMatch (p:ps) (s:ss) = mmap (s :) (match wildcard (p:ps) ss)



-- -- Test cases --------------------

-- testPattern =  "a=*;"
-- testSubstitutions = "32"
-- testString = "a=32;"

-- substituteTest = substitute '*' testPattern testSubstitutions
-- substituteCheck = substituteTest == testString

-- -- matchTest = match '*' testPattern testString
-- -- matchCheck = matchTest == Just testSubstitutions



-- -------------------------------------------------------
-- -- Applying patterns
-- --------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply _ _ [] _ = Nothing
transformationApply w function string tuple = 
  case match w (fst tuple) string of
        Just matched -> Just (substitute w (snd tuple) (function matched))
        Nothing -> Nothing

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply w function (tuple:tuples) string = 
  case transformationApply w function string tuple of
    Just matched -> Just matched
    Nothing -> transformationsApply w function tuples string

rulesApply transformations phrase =
  case transformationsApply "*" reflect transformations phrase of
    Just transformed -> transformed
    Nothing -> []
