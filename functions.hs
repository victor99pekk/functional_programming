


in_range min max x =
    min <= x && x <= max

fac n = 
    if n <= 1 then
        1
    else
        n * fac (n-1)

fib n = 
    if n <= 1 then
        1
    else
        fib (n-1) + fib (n-2)


data Calculation = 
    Add Int Int | Sub Int Int

calc :: Calculation -> Int
calc (Add x y) = x + y
calc (Sub x y) = x - y