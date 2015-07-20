\begin{code}
module Intro where
\end{code}




Hello World: The factorial Function
------------------------------------

- Product Definition
   
   n! =  1 * 2 * ... * n

- Recursive Definition

   n! = 1          , if n == 0
   n! = n * (n-1)! , if n >  0 

\begin{code}
fact n = if n == 0 then 1 else n * fact (n-1)
\end{code}


Alternative Syntax

\begin{code}
fact' 0 = 1
fact' n = n * fact'(n-1)
\end{code}


What is the type of fac?

fact :: Int     -> Int   
fact :: Double  -> Double
fact :: Real    -> Real
....


The type of `fact` can be `a -> a` for *any* type `a` that supports

* equality       (==)
* minus          (-)
* multiplication (*)


fact :: (Num a, Eq a) => a -> a


Exercise: Write the function `fibonacci`
https://en.wikipedia.org/wiki/Fibonacci_number



Data Types
-----------

What should happen if I call fact with a negative number?
Throw an error!!!

`factErr :: Int -> f`




\begin{code}
fib 0 = 0 
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
\end{code}





\begin{code}
hanoi :: Integer -> a -> a -> a -> [(a, a)]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
\end{code}