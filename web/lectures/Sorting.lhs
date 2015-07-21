\begin{code}
module InsertSort where
\end{code}


Insert Sort
===========

[Wiki Insert Sort](http://en.wikipedia.org/wiki/Insertion_sort)


How to implement insertSort in Haskell

* Try to implement the imperative algorithm -- **Never!!!**

* Break your algorithm in sub problems
  * insert an element into a sorted list
  * compine your subproblems


Step 1: Implement insert
-----------------------

`insert x xs` inserts the element `x` into the sorted list `xs`

\begin{code}
insert :: (Ord a) => a -> [a] -> a
insert = undefined 
\end{code}

Step 2: Use `insert` to implement sorting
------------------------------------------

Use `insert` to compine the elements of the initial unsorted list

\begin{code}
sort :: (Ord a) => [a] -> [a]
sort = undefined 
\end{code}


*Reminder:* Did you user `fold`?

When you want to combine list elements, with a function and a base case, 
it is suggested to use folds:


< foldr :: (a -> b -> b) -> b -> [a] -> b
< foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)



How to implement a function in Haskell
======================================


* Try to implement the imperative algorithm -- **Never!!!**

* Break your algorithm in sub functions
    - insert an element into a sorted list
    - compine your helper functions

  OR

* Write the high level implementation of your algorithm, 
and then fill in the details


QuickSort
=========

[Wiki QuickSort](http://en.wikipedia.org/wiki/Quicksort)

I put the pivot in the middle, 
the (sorted list of) elements that are less or equal than the pivot at the left, and
the (sorted list of) elements that are greater that the pivot at the left:


\begin{code}
quicksort' (x:xs) = leq_than_x ++ [x] ++ g_than_x
  where leq_than_x = undefined
        g_than_x   = undefined
\end{code}

How do I get the elements of `xs` that are less than `x`?

* Use filter

```
leq_than_x = filter (\y -> y <= x) xs
```

where `(\y -> y <= x)` is an anonymous function with argument `y` and body `y <= x` that behaves exactly like f:

```
f y = y <= x
```

OR 

* Use list comprehension

```
[y | y <- xs, y <= x]
```

\begin{code}
quicksort'' (x:xs) = leq_than_x ++ [x] ++ g_than_x
  where leq_than_x = quicksort [y | y <- xs, y <= x]
        g_than_x   = quicksort [y | y <- xs, y > x]
\end{code}


Finally putting everything in one line:

\begin{code}
quicksort []     = []
quicksort (x:xs) = (quicksort [y | y <- xs, y <= x]) ++ [x] ++ (quicksort [y | y <- xs, y > x])
\end{code}


In Haskell you *CAN* implement quicksort in one line!
Compare it with implementations in any other language....
Isn't this one more intuitive?
