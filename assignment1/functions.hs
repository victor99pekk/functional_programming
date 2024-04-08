
substitution _ "" _ = ""
substitution wildcard (x:xs) replaces 
    | x == wildcard = replaces ++ (substitution wildcard xs replaces)
    | otherwise     = [x] ++ (substitution wildcard xs replaces)

-- match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] _ = Just []  -- Base case: if pattern is exhausted, return empty list
match _ _ [] = Nothing -- Base case: if input string is exhausted but pattern remains, return Nothing

match wildcard (p:ps) (s:ss)
    | s == p = match wildcard ps ss
    | wildcard == p = string_builder wildcard (p:ps) (s:ss) [p]
    | otherwise = Nothing

-- string_builder :: Eq a => a -> [a] -> [a] -> [a] -> Maybe [a]
string_builder _ [] s string = Just (string ++ s) -- Base case: if pattern is exhausted, return the accumulated string
string_builder _ p [] string = Just (string ++ p)

string_builder wildcard (p:ps) (s:ss) string
    | p == s = iterator wildcard ps ss string -- Add character to string if they match
    | otherwise = string_builder wildcard ps ss (string ++ [s]) -- Replace wildcard in string

-- iterator :: Eq a => a -> [a] -> [a] -> [a] -> Maybe [a]
iterator _ [] _ string = Just string  -- Base case: if pattern is exhausted, return the accumulated string
-- iterator _ [] s string = Just (string ++ s) -- Base case: if pattern is exhausted, return the accumulated string
iterator _ p [] string = Just (string ++ p)
iterator wildcard (p:ps) (s:ss) string
    | p == s || p == wildcard = iterator wildcard ps ss string -- Skip character if it's the same as pattern or wildcard
    | otherwise = Nothing
