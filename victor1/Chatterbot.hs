-- Jonathan Do, Victor Mikkelsen
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use when" #-}
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

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind botbrain =
  fmap (\r -> rulesApply $ (map . map2) (id, pick r) botbrain) (randomIO :: IO Float)

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply tlist phrases = fromMaybe [] (transformationsApply "*" reflect tlist phrases)

reflect :: Phrase -> Phrase
reflect [] = []
reflect (p:ps) = maybe p id (lookup p reflections) : reflect ps

reflections =
  [ ("am", "are"),
    ("was", "were"),
    ("i", "you"),
    ("i'm", "you are"),
    ("i'd", "you would"),
    ("i've", "you have"),
    ("i'll", "you will"),
    ("my", "your"),
    ("me", "you"),
    ("are", "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your", "my"),
    ("yours", "mine"),
    ("you", "me")
  ]

---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (== "quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map (\(str, strs) -> (words $ map toLower str, map words strs))

--------------------------------------

reductions :: [PhrasePair]
reductions =
  (map . map2)
    (words, words)
    [ ("please *", "*"),
      ("can you *", "*"),
      ("could you *", "*"),
      ("tell me if you are *", "are you *"),
      ("tell me who * is", "who is *"),
      ("tell me what * is", "what is *"),
      ("do you know who * is", "who is *"),
      ("do you know what * is", "what is *"),
      ("are you very *", "are you *"),
      ("i am very *", "i am *"),
      ("hi *", "hello *")
    ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply = fix . try . transformationsApply "*" id

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wc (t:ts) s = if t == wc then s ++ substitute wc ts s else t : substitute wc ts s

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
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


-- | Apply a single pattern to a list of tokens.
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

