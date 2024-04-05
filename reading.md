# Haskell
- everyt datatype in haskell is immutable

# Functions

- haskell evaluate functions from the inside out, and evaluate them lazy.
- lazy evaluation means that haskell doesn't evaluate a function until we actually the need value from the function.

### Ex laze ev:
in_range min max x = min <= x && x <= max

- if we call the function in range with args 2 5 1 only we will inly evaluate wether the first statement holds, since it doesn't hold we dont need to compare the second.


### Types of functions

- the type of a function is a list of types. first the types for your args, and then the return type. 
- Types in `in_range` :: 
  Integer -> Integer -> Integer -> Bool


# Recursion

## No Loops in haskell
- there are no loops in haskell, things have to be done with recursion

### Guards
instead of using if and else you can use guards. You can use however many you want.
```haskell
fac n = 
    |n <= 1 = 1
    |otherwise = n * fac(n-1)
```
otherwise will always be true here

## Pattern matching
```haskell
is_zero 0 = true
is_zero _ = false
```
this definition would be partial if the second statement didnt exist

## Accumulators
- is used to implement tail recursion. This is good because compilers can rewrite this as loops.
- tail recursion is what we want to strive for, since regular recursion can cause stack overflow.
- the accumulator is a third argument to the function and is used to store the copmutation this far.
## ex:
```haskell
fac n = aux n 1
  where
    aux n acc
    | n <= 1    = acc
    | otherwise = aux (n-1) (n*acc)
```
- aux is in this case a helper function to fac, that takes to args.

# Lists
- lists can only have one internal type
- lists can either be constructed with in empty list:
  [ ]
  or by prepending an element to an already existing list:
  [1, 2, 3] = 1 : 2 : 3 : [ ]

### Generating list

```haskell
asc :: Int -> Int -> [Int]
asc n m = 
  | m < n   = []
  | m == n  = [m]
  | m > m   = n : asc (n+1) m
```

```haskell
asc 1 3 
  => 1 : asc 2 3 => 1 : 2 : asc 3 3
  => 1 : 2 : 3 : asc 4 3
  => 1 : 2 : 3 : []
```

- since list are very important there is a package named `Data.List` you can import

### important funciton in List-package
```haskell
head :: [a] -> a
head [1, 2, 3, 4]
=> 1
```

```haskell
tail :: [a] -> [a]
tail [1, 2, 3]
=> [2, 3]
```

```haskell
length :: [a] -> Int
```

```haskell
init :: [a] -> [a]
init [1, 2, 3]
=> [1, 2]
```
this one gives copy of list since datatypes are immutable in haskell

```haskell
null :: [a] -> Bool
null []
=> True
```

## functions with lists of Booleans
```haskell
and :: [Bool] -> Bool
and [True, False, True]
 => False

or :: [Bool] -> Bool
or [True, False, True]
=> True
```

<br>
### List Comprehension

[`operation` | `iter.element` <- `list` , `Guard`]

- Operation is the operation we want to perform on the elemtents of the list, example: 2*element.
- iter.element is the iterating element that iterates through the list
- the list is just the list
- the guard acts as a filter, and filters so we only get the desired elements of the list.
  
```haskell
[2*x | x <- [1, 2, 3], x > 1 ] 
=> [2, 4, 6]
```


### creating tuples with list comprehension
```haskell
[ (x,y) | x <- [1, 2, 3], y <- ['a', 'b'] ]
=> (1, a), (1, b), (2, a), (2, b), (3, a), (3, b)
```

### List patterns
```haskell
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs
```

```haskell
evens :: [Int] -> [Int]
evens [] = []
evens (x:xs)
  | mod x 2 == 0    = x : evens xs
  | mod x 2 != 0    = evens xs
```
- an important note is that we build a new list.

### Tuples
- (1, 2) :: (Int, Int)
- 2-tuples dont have to be the same type

pattern matching on tuples
```haskell
fst :: (a,b) -> a
fst(x,_) -> x

snd :: (a,b) -> b
snd(_,y) -> y
```


# Higher Order and Anonymous functions

```haskell
app :: (a -> b) -> a -> b
app f x = f x
```
ex:
```haskell
app (\x -> x+1) 1
=> 2
```
<br>

### (\ args -> expr), ex: (\x y z -> x+y+z)

example of a function that takes an argument, adds 1 and saves it:
```haskell
add1 = (\x -> x+1)
```
we have saved the function in the add1 variable. It is important to note that functions are seen as values as values in haskell, and thats why they can be given as arguments


## Map
in haskell you cant just apply a function to a list. thats where the map functino comes in.

while:
```haskell
newList = map double oldList
notWorking = double oldList
```
in the above example, only newList will be a list with every value doubled from oldList
```haskell
map :: (a -> b) -> [a] -> [b]
=> [a] -> [b]
=> [a, b, c,..,z] -> [1, 2, 3,.., n]
```
important to note is that the mappings can be of different types.

ex:
```haskell
map (\(x,y) -> x+y) [(1,2), (2, 2)]
=> [3, 4]
```

## Filter
```haskell
filter :: (a -> Bool) -> [a] -> [a]

filter (\x -> x > 2) [1, 2, 3]
=>[3]
```

# Currying
Currying is the process of transforming a function that takes several arguments, into one or more functions that take only one. All functions in haskell technicallt only take one argument.
```haskell
f :: a -> b -> c -> d
```
we can rewrite this as:
```haskell
f :: a -> (b -> (c -> d))
```
a function that always only takes one argument

Example of currying (add):
```haskell
add :: Int -> Int -> Int
add x y = x+y
add x = (\y -> x+y)
add = (\x -> (\y -> x+y))
```
## Partial function application
what happens when we only give add for example one argument?
```haskell
add 1 :: Int -> Int
=> A new Function of (Int -> Int)
```
- `add 1` will be equal to (\x -> x+1)

this will give us a new function rather than a value like it does when we supply two arguments. This is what partial function application is.

### creating a partial function with map
```haskell
map :: (a -> b) -> a -> b
doubleList = map(\x -> 2*x)
```


# Function composition
```haskell
(f . g) equiv. to (\x -> f (g x))
```
first apply the funciton g then f.

- if we for example have a function that sort in ascending order. but we want a function that sort in descending order. We can compose such a function from a reverse and a sort (ascending) function.
Like this:

```haskell
descSort = reverse . sort
```

<br>

### mappings
we can only use compositions in mappings:
```haskell
map2D :: (a -> b) -> [[a]] -> [[b]]
map2D = map . map

map2D f xs = map (\ys -> map f ys) xs
```
we can see that the argument to map2D must be a list of lists.


## Dollar sign
dollar sign is like a parenthesis. it is used to clean up the syntax
```haskell
($) :: (a -> b) -> a -> b
```

# Folding
is commonly use when working with lists.

typically a fold deals with three things, a combining function and a datastructure and an accumulator. The foldr function breakes down the datastructure starting to apply the function to the rightmost value in the datastructure and the accumulator.
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr 54 (-) [10, 11, 5, 12]
=> 53
```

to understand the folder we can start by rewriting the list
```haskell
10:(11:(5:(12:[])))
```
applying `foldr function` will replace the empty list with the starting value of the accumulator. and the ":" with the function being applied.
we can therefore rewrite the expression above as:

```haskell
10-(11-(5-(12-(54))))
```

the foldr needs:
```haskell
foldr (\x acc -> term) start_acc list
```

we can for example build a function that counts the occurences of an element in a list using foldr:
```haskell
count = 
        foldr (\x acc -> if x == element then acc else acc) 0

isAll e = foldr (\x acc -> x==e && acc) True

lengthFunction = foldr (\x -> (+) 1) 0
lengthFunction = foldr (\const $ (+) 1) 0
```

## Direction
the direction switches how you should provide the function arguments.
```haskell
foldr (\element acc -> term) acc list

foldl (\acc element -> term) acc list
```

# Datatypes
defining a datatype works as follows:
```haskell
data Name = Constructor1 args | Constructor2 args | ...

data Calculation = 
    Add int int | Sub int int | Mul int int | Div int int

calc :: Calculation -> int
calc (Add x y) = x+y
calc (Sub x y) = x-y
calc (Mul x y) = x*y
calc (Dic x y) = div x y
```
here we use pattern matching.


# Records
records help defining what types the arguments of a datatype is.
Example:
```haskell
data Person = Person{name :: String,
                      age :: Int}
```
from this we automatically get:
```haskell
name :: Person -> String
age :: Person -> Int
```
We can now write a function that greets a person:
```haskell
greet person = "hi " ++ name person
```


# Typeclasses
typeclasses are somewhat analogous to interfaces in object-oriented programming. 

for a type to be an instance of a typeclass, it has to implement the functions specified by the typeclass.

this allows functions to be written in a generic way.

Example:
```haskell
data Temp = C Float | F Float

instance Eq Temp where
  (==) (C n) (C m) =  (n == m)
  (==) (F n) (F m) =  (n == m)
  (==) (F f) (C c) = (1.8*c + 32) == f
  (==) (C c) (F f) = (1.8*c + 32) == f
```

# Maybe datatype
```haskell
data Maybe a = Nothing | Just a
```
if there is an error in a function call, what should the function return?

Maybe is used to solve this. say we have a function f with an argument x:
```haskell
f x
```
if the function cant perform an operation (eg: divide by 0) what should we return?

If we return always return a maybe, it can either contain:
```haskell
Just result
```
or
```haskell
Nothing
```
if an exception has occurred

Example:
```haskell
safeDiv :: Integral a => a -> a -> Maybe a

safeDiv a b =
              if b == 0 then Nothing
              else Just $ div a b
```
helpful functions in Data.Maybe
```haskell
isJust :: Maybe a -> Bool
isNothing :: Maybe a -> Bool
fromJust :: Maybe a -> a
```

here is how we safely can get the value from a maybe.
```haskell
fromMaybe :: a -> Maybe a -> a
fromMaybe default (Nothing)
=> default
fromMaybe default (Just a)
=> a
```


# Input - Output
first versions of haskell couldn't outpout anything

save a string:
```haskell
hw = putStrLn "Hello World!"
hw
=> "Hello World!"
```
putStrLn is a function that takes a String and return an IO-action, which is to print the string.

```haskell
getLine :: IO String

greet :: IO()
greet = do
    putStrLn "your name?"
    name <- getLine
    putStrLn ("Hello " ++ name)
```

it is possible to have arguments in the io action:
```haskell
count :: Int -> Int -> IO()

count n m = do
          putStrLn show(n)
          if n < m
            count (n+1) m
          else
            return ()
```


# Type interference
type intererence refers to the ability of the compiler to automatically deduce the types of expressions and functions, without them being annoted.

- The question is now how do we keep it as general as possible?
  

1. Assign every variable a unique typevariable
2. Assign every function it's type with new unique typevariables
3. for each subexpression of the expression generate equations of types
4. resovle the equations until no further simplifications can be done. Conflicting types imply a type error otherwise the type has been infered.


# Monad

### bind >>=
```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b

Just 1 >>= (\x-> Just x)
==> Just 1

Nothing >>= (\x-> Just x)
==> Nothing
```

```haskell
maybeadd :: Num b => Maybe b -> b -> Maybe b
mybeadd mx y -> mx >>= (\x -> Just $ x+y)

maybeadd Nothing 1
==> Nothing
maybeadd (Just 1) 1
==> Just 2
```

Example of instansiation of Monad:
```haskell
instace Monad Maybe where
  m >>= f = case m of 
              Nothing -> Nothing
              Just x -> f x
  return v = Just v
```


