\begin{code}
module Intro where

import Prelude hiding (length, head, tail, (++))

import Data.Char (toLower)
\end{code}




Hello World: The factorial Function
------------------------------------

Factorials are very simple things. They're just products, indicated by an exclamation mark. For instance, "four factorial" is written as "4!" and means 1×2×3×4 = 24. In general, n! ("enn factorial") means the product of all the whole numbers from 1 to n; that is, n! = 1×2×3×...×n.

- Examples
```  
    1! = 1
    2! = 1 * 2 = 2
    3! = 1 * 2 * 3 = 6
```
- Product Definition

```   
    n! =  1 * 2 * ... * n
```

- Recursive Definition

```
    n! = 1          , if n == 0
    n! = n * (n-1)! , if n >  0 
```

- Haskell implementation

\begin{code}
fact :: Int -> Int 
fact n = if n == 0 then 1 else n * fact (n-1)
\end{code}


Alternative Syntax

\begin{code}
fact' 0 = 1
fact' n = n * fact'(n-1)
\end{code}


Yet, another Haskell syntax
\begin{code}
fact'' n 
  | n <= 1 = 1
  | otherwise = n * fact'' (n-1)
\end{code}



Exercise 1
----------

In mathematics, the Fibonacci numbers or Fibonacci sequence are the numbers in the following integer sequence:

```
  1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, ...
```

By definition, the first two numbers in the Fibonacci sequence are 1 and 1, and each subsequent number is the sum of the previous two.
In mathematical terms, the sequence Fn of Fibonacci numbers is defined by the recurrence relation


```
    F 1 = 1
    F 2 = 1
    F n = F (n-1) + F (n-2)
```

Write the fibonacci function!

Exercise 2
----------
The Towers of Hanoi is a puzzle where you are given three pegs, on one of which are stacked `n` discs in increasing order of size. 
To solve the puzzle, you must move all the discs from the starting peg to another by moving only one disc at a time and never stacking a larger disc on top of a smaller one.
To move `n` discs from peg a to peg b using peg c as temporary storage:

- Move n - 1 discs from peg a to peg c.
- Move the remaining disc from peg a to peg b.
- Move n - 1 discs from peg c to peg b.

Write a function

```
hanoi :: Int -> String -> String -> String -> IO ()
hanoi = error "Define me!"
```
that, given the number of discs n and peg names a, b, and c, where a is the starting peg, emits the series of moves required to solve the puzzle. For example, running hanoi 2 "a" "b" "c"

should emit the text

```
[("a","c"),("a","b"),("c","b")]
```



Basic Type Information
---------------------- 

What is the type of fac?

```
fact :: Int     -> Int   
fact :: Double  -> Double
fact :: Real    -> Real
....
```

The type of `fact` can be `a -> a` for *any* type `a` that supports

* equality       `(==)`
* minus          `(-)`
* multiplication `(*)`



Equality is implemented with an interface (`class` in Haskell terms) we call `Eq`
and numeric operations by `Num`. 
Thus, the type of `fact` is from- and to any type (variable) `a` that
_instantiates_  `Num` and `Eq`.

```
fact :: (Num a, Eq a) => a -> a
```


Data Types
-----------
What should happen if I call `fact` with a negative number?
_Throw an error!!!_


If the argument is negative (n < 0) factorial is not defined.
Build a data structure (`ErrInt`) that tags the result of factorial as

* Error n (for negative `n`s)
* Value n (for the real result)

\begin{code}
data Err a = Error a | Value a
\end{code}

Note: When you want to use the result of the factorial you need to case analyse
to distinguish if it is an Error or a Value:

\begin{code}
factErr :: Int -> Err Int 
factErr n 
  | n == 0    = Value 1
  | n >  0    = case factErr (n-1) of 
  	            Value m -> Value $ n * m
  	            Error m -> Error m
  | otherwise = Error n
\end{code}




What is that dollar? (`$`)
---------------------------
It is a "silly" function commonly used in Haskell to avoid parenthesis. 
When you see `f $ e` you can replace it with parenthesyzing the right part of dollar: `f (e)`
Dollar is defined as:

\begin{code}
infixr 0 $$ 
f $$ x = f x
\end{code}


Lists
----------
We saw `Err a` as a data type that wraps values of type `a`.
The most used Haskell data type is a list

< [1, 2, 3]            :: [Int]
< [True]               :: [Bool]
< ['c', 'h', 'a', 'r'] :: [Char] 
< "char"               :: String

Exercise
--------
What is the type of the empty list

< [] :: ??

List operations

- Getting the length of a list

\begin{code}
length :: [a] -> Int
length []     = 0 
length (x:xs) = 1 + length xs
\end{code}

< length [1, 2, 3, 4] = 4
< length []           = 0
< length "string"     = 6


Note: `length` is _polymorphic_ it operates on lists of every type.     


- Getting the head of a list
\begin{code}
head :: [a] -> a
head (x:_) = x
head []    = error "head on empty list"
\end{code}

Exercise
--------

Get the tail of a list

< tail "Mexico!!!!!" = "exico!!!!!"
< tail [1, 2, 3, 4]  = [2, 3, 4]

\begin{code}
tail :: [a] -> [a]
tail = undefined
\end{code}

Exercise
--------

Concatinate two lists

< "I love " ++ "Mexico!!!!!" = "I love Mexico!!!!!"
< [-1, 0]   ++ [1, 2, 3, 4]  = [2, 3, 4]

\begin{code}
(++) :: [a] -> [a] -> [a]
[] ++ xs = xs
\end{code}



Computation Patters: mapping
-----------------------------
Lets write a function that converts a string to uppercase. 
Recall that in Haskell, a String is just a list of Char. 
We must start with a function that will convert an individual Char to its uppercase version. Once we find this function, we will simply jog over the list, and apply the function to each Char.

How might we find such a transformer? Lets query Hoogle for a function of the appropriate type! Ah, we see that the module Data.Char contains a function.

< toLower :: Char -> Char

and so now, we can write the simple recursive function

< toLowerString :: String -> String
< toLowerString []     = []
< toLowerString (c:cs) = toLower c : toLowerString cs


Lets now write a function that given a list of integers 
increases each of its elements by 1

< plusOneList :: [Int] -> [Int]
< plusOneList []     = []
< plusOneList (n:ns) = (n+1) : plusOneList ns


Now, in a lesser language, you might be quite happy with the above code. But what separates a good programmer from a great one, is the ability to abstract.

Like humans and monkeys, the functions `toLowerString` and `plusOneList` share 93% of their DNA — the notion of jogging over the list. The common pattern is described by the polymorphic higher-order function map

```
map f []     = []
map f (x:xs) = (f x) : (map f xs)
```

How did we arrive at this? Well, you find what is enshrine in the function’s body that which is common to the different instances, namely the recursive jogging strategy; and the bits that are different, simply become the function’s parameters! Thus, the map function abstracts, or if you have a vivid imagination, locks up in a bottle, the extremely common pattern of jogging over the list.

Verily, the type of map tells us exactly what it does

```
map :: (a -> b) -> [a] -> [b]
```

That is, it takes an `a -> b` transformer and list of a values, and transforms each value to return a list of b values. We can now safely reuse the pattern, by instantiating the transformer with different specific operations.

\begin{code}
toLowerString = map toLower
plusOneList   = map (+1)
\end{code}

Much better.

Mapping `Err` values
--------------------

Write a function that increases the value of the `Err` data by one.

< plusOneErr :: Err Int -> Err Int
< plusOneErr = undefined

Did you follow the mapping abstraction discussed above?
If so, you would define a mapping function for `Err` values

\begin{code}
mapErr :: (a -> b) -> Err a -> Err b
mapErr f (Error x) = Error $ f x 
mapErr f (Value x) = Value $ f x 
\end{code}

\begin{code}
plusOneErr :: Err Int -> Err Int
plusOneErr = mapErr (+1)
\end{code}


Then, use `mapErr` to write the desired function.


We take the abstraction one level up!
See how `map` and `mapErr` are similar

< map    :: (a -> b) -> [a] -> [b]
< mapErr :: (a -> b) -> Err a -> Err b

< class Functor f where
< 	fmap :: (a -> b) -> f a -> f b




Answers to the Exercises
------------------------

\begin{code}
fib 0 = 0 
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
\end{code}





\begin{code}
hanoi :: Int -> String -> String -> String -> [(String, String)]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
\end{code}