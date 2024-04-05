
substitution _ "" _ = ""
substitution wildcard (x:xs) replaces 
    | x == wildcard = replaces ++ (substitution wildcard xs replaces)
    | otherwise     = x ++ (substitution wildcard xs replaces)


match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ _ []  = Nothing
match _ [] _  = Nothing
match wildcard (p:ps) (s:ss)
    | p == wildcard = matchWildcard ps (s:ss)
    | p == s        = match wildcard ps ss >>= \r -> Just (s:r)
    | otherwise     = Nothing

-- Helper function to match wildcard in the pattern
matchWildcard :: Eq a => [a] -> [a] -> Maybe [a]
matchWildcard _ [] = Nothing
matchWildcard pattern@(p:ps) s@(x:xs)
    | match p pattern s /= Nothing = Just s
    | otherwise                    = matchWildcard pattern xs
