in_range min max x = min <= x && x <= max

main :: IO ()
main =
    print $ in_range 4 10 2


