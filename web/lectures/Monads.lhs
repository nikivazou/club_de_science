\begin{code}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Monads where
\end{code}

Let's talk about Monads
=======================

One of the greatest features of Haskell is that it has _programmable semicolon_, 
i.e., the meaning of the semicolon is determined by the programmer.

For example, what is the meaning of the following function?

\begin{code}
compute e1 e2 = 
	do { x <- e1 
	   ; y <- e2
	   ; return $ x + y}
\end{code}

`compute` uses `e1` to pull the value `x`
uses `e2` to drug the value `y`
and returns the sum of `x` and `y`.

What is the type of compute?

\begin{code}
compute :: (Num a, Monad m) => m a -> m a -> m a
\end{code}

`e1` and `e2` wrap values of type `a` and since 
the last line is adding `a`s, `a` should be Numeric.
Moreover, the wrapper `m` should should instantiate `Monad`s
i.e., it should provide a method to pull a value and to return a value.


Lists are Monads
-----------------

Since lists are Monads and Integers are Numerics,
we can call `compute` with a list of integers. 

< compute [1, 2] [3, 4] == [4, 5, 5, 6]

We got the sum of each pair combination!!!!

< compute [1, 2] [3, 4] == [1 + 3, 1 + 4, 2 + 3, 2 + 4]

So, the way lists provide to pull their elements is give each one if them


Maybe is a Monad
-----------------

Thus we can call `compute` with Maybe values.

< compute (Just 1) (Just 3) == Just 4

Pulling values from `Just` values just returns the value. 
What about `Nothing`?

< compute Nothing  (Just 3) == Nothing
< compute (Just 1) Nothing  == Nothing

Nothing does not have a value to give, so the computation just returns Nothing!


Defining the Semantics of The Semicolon
---------------------------------------

To define the semantics of the semicolon for each type constructor (like list or Maybe)
you just need to define how the constructor instantiates the Monad class.

The class has just two methods, the bind and the return:

< class Monad m where
<   (>>=)  :: m a -> (a -> m b) -> m b
<   return :: a -> m a

Question: Have you seen the bind symbol `>>=` before?

The bind says how to pull values out of the constructor 
and the return says how to wrap values in the constructor. 

How do lists instantiate Monads? Just follow the types!

< instance Monad [] where
< 
<   -- return :: a -> [a]
<	return x = [x] 
< 
<   -- (>>=) :: [a] -> (a -> [b]) -> [b]
<   xs >>= f = concatMap f xs

< concatMap f []     = []
< concatMap f (x:xs) = f x ++ concatMap f xs


And how does this gives semantics to `compute`?

Every time the Haskell compiler seems semicolon (or the `do` notation)
it applies the follow _syntactic_ transformation

< compute :: (Monad m, Num a) => m a -> m a -> m a 
< compute e1 e2 =                compute e1 e2 =  
< 	do { x <- e1            ->       e1 >>= \x ->  
< 	   ; y <- e2                     e2 >>= \y -> 
< 	   ; return $ x + y}             return $ x + y


What bind (`>>=`) and return is the translation using?
The ones that we defined for the specific monads!!!!


So, for our list example, `compute [1, 2] [3, 4]` becomes

< [1, 2] `concatMap` \x -> 
< [3, 4] `concatMap` \y -> 
< [x + y]
< 
< [1, 2] `concatMap` \x -> 
< [x + 3] ++ [x + 4] ++ []
< 
< [1, 2] `concatMap` \x -> 
< [x + 3, x + 4]
< 
< [1, 2] `concatMap` \x -> 
< [1 + 3, 1 + 4] ++ [2 + 4, 2 + 4] ++ []
< 
< [1, 2] `concatMap` \x -> 
< [5, 5, 5, 6]


Defining the Semantics of Maybe Semicolon
------------------------------------------
Once again, to define the semantics of the maybe semicolon we just need to define
the monadic instance.


< instance Monad Maybe where
<  -- return :: a -> Maybe a
<  return x = Just x
< 
< -- (>>=) :: Maybe a ->  (a -> Maybe b) -> Maybe b
<  Nothing >>= _ = Nothing
< (Just x) >>= f = f x



Exercise: Remember the `Err` type?

\begin{code}
data Err a = Error a | Value a
\end{code}

Make `Err` an instance of Monad and use it to call the `compute` function!


The probability monad
----------------------

A probability is just a float number that should be between 0 and 1.

\begin{code}
data Prob = Prob Float  deriving (Eq, Ord, Show)
\end{code}

We can now define `Perhaps`, which represents a value with an associated probability:

\begin{code}
data Perhaps a = Perhaps a Prob
  deriving (Show)
\end{code}

Now, this is just a generalization of Haskell's built-in `Maybe` type, which treats a value as either present (probability `1`) or absent (probability `0`). All we've added is a range of possibilities in between: `Perhaps x 0.5` represents a 50% chance of having a value.

\begin{code}
instance Monad Perhaps where
  return x            = undefined 

  (Perhaps x p) >>= f = undefined 
\end{code}

Now you can use `Perhaps` as a monad and the meaning of the semicolon is to 
multiply probabilities!!!!!

\begin{code}
pcompute = do 
	x <- Perhaps 1 $ Prob 0.1
	y <- Perhaps 3 $ Prob 0.3
	return $ x + y
\end{code}

< pcompute == Perhaps 4 (Prob 3.0000001e-2)

Magic!

What if you want to combine probabilities with another monad?
Say that you want to have a list of probabilities and you want your bind
operator (or the semicolon notation) to both pull each element of the list
_and_ multiply the probabilities?
Turns out you can have this feature! Coming soon!
Before that, let's see how monads are used to fake effectful programming in Haskell!



Cheating!!!!!
--------------
< instance Monad Perhaps where
<   return x = Perhaps x 1
< 
<   (Perhaps x p) >>= f = Perhaps y (p * p')
<     where (Perhaps y p') = f x

\begin{code}
instance Num Prob where
  (Prob n1) + (Prob n2) = Prob (n1 + n2)
  (Prob n1) - (Prob n2) = Prob (n1 - n2)
  (Prob n1) * (Prob n2) = Prob (n1 * n2)
  abs (Prob n)          = Prob (abs n)
  signum (Prob n)       = Prob (signum n)
  fromInteger n         = Prob (fromInteger n)
   
\end{code}