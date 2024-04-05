
concatenateStrings :: String -> String -> String
concatenateStrings str1 str2 = str1 ++ str2


substitution _ "" _ = ""
substitution wildcard (x:xs) replaces 
    | x == wildcard = replaces ++ (substitution wildcard xs replaces)
    | otherwise     = x ++ (substitution wildcard xs replaces)