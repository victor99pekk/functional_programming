
# Lecture Notes

## Sequecing I/O

- >> is used when the value from the first operation is uniteresting.
- >>= passes the first operation as an argument to the second.

```haskell
(>>=) :: IO a -> (a -> IO b) -> IO b 
(>>) ::IOa->IOb ->IO b
```

### Example:
```haskell
main = readFile "infile" >>= \ s -> writeFile "outfile" (filter isAscii s) >>
       putStr "Filtering successful\n"
```
main here takes an `infile` and takes all ascii characters from that file and prints them to
to the `outfile`. After that it prints "filtering complete" to the terminal output.

Here is an other way of writing the same thing:
```haskell
main = do
        putStr "Input file: "
        ifile <- getLine
        putStr "Output file: "
        ofile <- getLine
        s <- readFile ifile
        writeFile ofile (filter isAscii s)
        putStr "Filtering successful\n"
```


- These two examples are equivalent:
```haskell
echoReverse = do
    aLine <- getLine
    putStrLn (reverse aLine)

echoReverse =
    getLine >>= \aLine ->
    putStrLine (reverse aLine)
```


## Modules 

- Each haskell program is a collection of modules.
- one module must be called main and export value main

### exporting from module
- each module declares what entities it export, with the defualt being set to all.

Example:
```haskell
-- exporting entities exportingA and exportingB from module
ModuleA (exportingA, exportingB) 

-- exporting all entities
ModuleB
```

- Import works the same way, only then is it the module thats importing that defines what entitites to import, default is ALL.

- qualified keyword forces you to type the module name before using the module's imported entities. 
```haskell
import qualifed moduleB as moduleA
moduleA.entityA
```

## curry
- currying means taking a function that takes n arguments and giving it between [1, (n-1)] arguments. This gives a new function take takes less arguments than the first one.


## Precedence

infixr 9 .
infixr 8 ^, ^^, ..
-- (..) is a built-in syntax!
infixl 7 ., /, ‘quot‘, ‘rem‘, ‘div‘, ‘mod‘
infixl 6 +, -
-- The (:) operator is built-in syntax, and cannot legally -- be given a fixity declaration; its fixity is given by: -- infixr5 :
infix 4 ==, /=, <, <=, >=, >
infixr 3 &&
infixr 2 ||
infixl 1 >>, >>=
infixr 1 =<<
infixr 0 $, $!, ‘seq‘


### data : Either

data Either a b = Left a | Right b
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left a) = f a
either f g (Right b) = g b

Example:
```haskell

isNull :: Either String Integer -> Bool
isNull = either (=="") (==0)
```

## Functions on lists
### Zip functions
```haskell
unzip    :: [(a, b)] -> ([a], [b])
unzip [(’a’,’b’),(’c’,’d’)] = ("ac","bd")
zipWith ::(a->b->c)->[a]->[b]->[c] zipWith (+) [1,2] [3,4] = [4,6]
zip3     :: [a] -> [b] -> [c] -> [(a, b, c)]
unzip3   :: [(a, b, c)] -> ([a], [b], [c])
```
```haskell
repeat           :: a -> [a]
repeat ’a’ = "aaaaaaaaa..."

replicate        :: Int -> a -> [a]
replicate 4 ’a’ = "aaaa"

cycle            :: [a] -> [a]
cycle "abc" = "abcabcabc ..."

iterate :: (a -> a) -> a -> [a] iterate(++"")""=["",""," ",...]
```


## Type inference/declaration
### Why do we need types if they only restrict us?
- the compiler helps us to avoid easy misstakes because it tells us when we are doing something we can't do
- Type delcaration allows the compiler to know more about the code which allows for better optimization.

### creating new types
this only creates an alias for String (they can be used interchangeably):
```haskell
type MySuperCoolString = [Char]
```

this actually creates a new type:
```haskell
newtype MyEvenCoolerString = CoolString String

s = CoolString "Hello"
```
CoolString is called the constructor, and String is the field.
funcitonallt speaking our newtype is in this case a string, but the compiler considers it different from the String, meaning we can't use them interchangeably.

To access the string in our newtype you have to unwrap it first

### Data
- data is more powerful than newtype, but the compiler can optimize the code more when newtype is used. It therefore better to use newtype when possible.
- The main difference between the two is that data can have [0, inf] number of constructors, where as the newtype must exactly one.
Example:
```haskell
data Season = Spring | Summer | Autumn | Winter
x = Summer
data SeasonTemp t = SpringT t | SummerT t | AutumnT t | WinterT t
x = AutumnTemp 15
```
t is like a template in this case, we can put anything as t.

### Classes
- The word class in Haskell refers to the creation of a class of types, hence we call it a typeclass.

Example:
```haskell
class Set t where
    class_insert :: Ord a => t a -> a -> t a 
    class_contains :: Ord a => t a -> a -> Bool
```
the typeclass set here is defined to contain all types that can be said to describe a set.
The functinos in the typeclass are the functions that have to be implemented for a type to be considered part of the class.

- We can tell the compiler that a data type is part of a typeclass by using instance of as oin the example below:
Example:
```haskell
 instance Set Tree where 
    class_insert = insert 
    class_contains = contains
```
insert and contains are already implemented functions, and can therefore easily be reused here.

### Basic types in haskell
Bool
Char
String
Int
Integer
Float
Double


## Class inheritance
```haskell
class Graphical a => Enclosing a where
   encloses :: Point -> a -> Bool
```
inorder for a typeclass to now be a part of the Enclosing typeclass, it must first be a
part of the graphical typeclass. A class has to be a a part of the Graphical typeclass to
be part of the enclosing typeclass in this case.


### Comparison to Java

Haskell types <-> Java classes

Haskell class <-> Java interface


## Functor
- functor is a mathematical concept aswell, and in haskell is a typeclass for all types that 
implements the fmap function.
- Functors abstract the idea of mapping a function over each element in a structure
```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

And here is how Maybe implements the fmap to be part of the functor typeclass:
```haskell
data Maybe a = Nothing | Just a

instance Functor Maybe where
    --fmap :: (a -> b) -> Maybe a -> Maybe b fmap _ Nothing = Nothing
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)
```
### functor math-laws
functors are required to satisfy 2 mathematical laws:
```haskell
-- The identity law
    fmap id = id

-- The composition law
    fmap (g . h) = fmap g . fmap h
```
- The identity law states that if we apply the identity function to a functor we
should get the functor unchanged
- The composition law states that mapping the composition of two functors is the same as first mapping one function, and then mapping another.

### Applicatives
Applicatives ⊂ Functors
- All applicatives are functors

- applicatives generelize functors to to use several arguments
```haskell
instance NaiveApplicative f where
fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap4 :: (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
```
This is a anive way of implementing the fmap to take several number of arguments

- Using the idea of currying, a version of fmap with any number of arguments can be constructed in terms of two basic functions with the following types:
```haskell
pure :: a -> f a

(<*>) :: f (a -> b) -> f a -> f b
```
- Pure converts a value of type a into a structure of type "f a"
- <*> is a generalized form of function application for which the argument-function, argument-value, and the result are all contained within the f-structures.

Example:
```haskell
g <*> x <*> y
```
becomes:
```haskell
((g <*> x) <*> y)
```
When the two are used together it looks like this:
```haskell
pure g <*> x1 <*> x2 ... <*> xn
```

Example of applicative implementation
```haskell
class Functor f => Applicative f where 
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

-- list is part of functor and can therefore implement applicative
instance Applicative [] where 
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
```


### Monad
Monads ⊂ Applicatives ⊂ Functors

```haskell
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where 
    return x = Just x
    x >>= f = bind x f
```

- Monads aren't really generlizations of the same patterns as applicatives, but rather an extension
- the bind function (>>=) is is stringly associated to the monad. it takes the result value from the left function and passes it to the right function.


## Evaluation

- A `redex` is a reducable expression. There are many different orders of evaluations possible that all produce the same result.

Example of redex:
```haskell
2 + 3 (reducing) => 5
```

there are 2 major order of evalutaions, they are `call-by-value` and `call-by-name`

### call by value
in call by value we start from the inside.

Example
```haskell
mult (1+2, 2+3)
=   {applying the first +}
    mult(3, 2+3)
=   {applying second +}
    mult(3, 5)
= {applying mult 3 5}
    3 * 5
= {applying *}
    15
```
call by value ensures that the arguments are fully evaluated before they get passed to the function. They have to be since it is their value you call, and not their name.

### call by value
in outermost evaluation we start from the opposite side of innermost evaluation. the last example gets evaluated in the following way in innermost evaluation.

Call-by-name is better the call-by-value, as outermost produces results in some cases when innermost fails to.

- if there exist an evaluation sequnce in call-be-value, there exist one in call-by-name, the converse is not true. call-by-name is therefore used in haskell

```haskell
mult (1+2, 2+3)
=   {applying mult}
    (1+2) * (2+3)
=   {applying the first +}
    3 * (2+3)
=   {applying second +}
    3 * 5
= {applying *}
    15
```
one thing worth noting is that numeric operators such as (+, -, *, /) can not be evaluated until their arguments are evaluated

Functions like + and * that need to arguments that are numbers are called strict.

## number of reductions in evaluations

- if one argument is used several times, the outermost evaluation will require more evaluations than innermost evaluation. This is because the innermost will only evaluate the argument once, before the operations in the function have been applied, whereas the argument will be evaluated continously in outermost as it's value is needed.

Outermost:
```haskell
square (1+2)
=   {applying square}
    (1+2) * (1+2)
=   {applying +}  --first eval of argument
    3 * (1+2)
=   {applying +} -- second eval of same argument
    3 * 3
= {applying *}
    9
```

innermost:
```haskell
square (1+2)
=   {applying +}  --first and only eval of argument
    square 3
=   {applying square}
    3 * 3
= {applying *}
    9
```

- fourtunatly the call-be-name evaluation ineffiency problem can be easily solved. It is solved by using pointers that indicate sharing of expressions during the evaluation. That is, instead of physically copying the argument we have several pointers to the address where the value lies.

### lazy evaluation
The combination of copying and call-by-name evaluation is what is known as lazy-evaluation and is what is used in haskell

## Strict Application ($!)
- to avoid avoid lazy evaluation you can use the "$!" operator. it ensures that function arguments are fully evaulated when the function is applied.
