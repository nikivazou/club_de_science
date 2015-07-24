The State Monad
===============

\begin{code}
import Data.Map
\end{code}

Recap: Monads
-------------


The notion of a monad can now be captured as follows:

```
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
```

That is, a monad is a parameterised type `m` that supports `return` and `>>=` functions of the specified types. 
The fact that `m` must be a parameterised type, rather than just a type, is inferred from its use in the types for the two functions. Using this declaration, it is now straightforward to make `Maybe` into a monadic type:

```
instance Monad Maybe where
   -- return      :: a -> Maybe a
   return x       =  Just x

   -- (>>=)       :: Maybe a -> (a -> Maybe b) -> Maybe b
   Nothing  >>= _ =  Nothing
   (Just x) >>= f =  f x
```

(Aside: types are not permitted in instance declarations, but we include them as comments for reference.) It is because of this declaration that the do notation can be used to sequence `Maybe` values. More generally, Haskell supports the use of this notation with any monadic type. In the next few sections we give some further examples of types that are monadic, and the benefits that result from recognising and exploiting this fact.



Imperative Functional Programming
----------------------------------

Consider the following problem. I have a (finite) list of values, e.g.

\begin{code}
vals0 :: [Char]
vals0 = ['d', 'b', 'd', 'd', 'a']
\end{code}

that I want to canonize into a list of integers, where each distinct value gets the next highest number. So I want to see something like

```
ghci> canonize vals0 
[0, 1, 0, 0, 2]
```

similarly, I want:

```
ghci> canonize ["zebra", "mouse", "zebra", "zebra", "owl"] 
[0, 1, 0, 0, 2]
```


_Exercise:_ How would you write canonize in Haskell?

Now, lets look at another problem. Consider the following tree datatype.

```
data Tree a = Leaf a 
            | Node (Tree a) (Tree a)
            deriving (Eq, Show)
```

Lets write a function

```
leafLabel :: Tree a -> Tree (a, Int)
```


that assigns each leaf a distinct integer value, so we get the following behavior

```
leafLabel (Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
  ==              (Node (Node (Leaf ('a', 0)) (Leaf ('b', 1))) (Leaf ('c', 2)))
```

_Exercise_ How would you write leafLabel in Haskell?


The State Monad
----------------

Now let us consider the problem of writing functions that manipulate some kind of state, represented by a type whose internal details are not important for the moment:

```
type State = ... 
```

The most basic form of function on this type is a state transformer (abbreviated by `ST`), which takes the current state as its argument, and produces a modified state as its result, in which the modified state reflects any side effects performed by the function:

```
type ST = State -> State
```

In general, however, we may wish to return a result value in addition to updating the state. For example, a function for incrementing a counter may wish to return the current value of the counter. For this reason, we generalise our type of state transformers to also return a result value, with the type of such values being a parameter of the ST type:

```
type ST a = State -> (a, State)
```

Such functions can be depicted as follows, where $s$ is the input state, $s'$ is the output state, and $v$ is the result value:

![monads](../static/monad1.png)

The state transformer may also wish to take argument values. However, there is no need to further generalise the `ST` type to take account of this, because this behaviour can already be achieved by exploiting currying. For example, a state transformer that takes a character and returns an integer would have type `Char -> ST Int`, which abbreviates the curried function type

```
Char -> State -> (Int, State)
```

depicted by:

![monads](../static/monad2.png)

Returning to the subject of monads, it is now straightforward to make `ST` into an instance of a monadic type:

```
instance Monad ST where
   -- return :: a -> ST a
   return x  =  \s -> (x,s)

   -- (>>=)  :: ST a -> (a -> ST b) -> ST b
   st >>= f  =  \s -> let (x,s') = st s in f x s'
```

That is, `return` converts a value into a state transformer that simply returns that value without modifying the state:

![monads](../static/monad3.png)

In turn, `>>=` provides a means of sequencing state transformers: `st >>= f` applies the state transformer st to an initial state `s`, then applies the function `f` to the resulting value `x` to give a second state transformer `(f x)`, which is then applied to the modified state `s`' to give the final result:

![monads](../static/monad4.png)


Note that `return` could also be defined by `return x s = (x,s)`.
However, we prefer the above definition in which the second argument `s` is shunted to the body of the definition using a lambda abstraction, because it makes explicit that return is a function that takes a single argument and returns a state transformer, as expressed by the type `a -> ST a`: A similar comment applies to the above definition for `>>=`.

We conclude this section with a technical aside. In Haskell, types defined using the type mechanism cannot be made into instances of classes. Hence, in order to make ST into an instance of the class of monadic types, in reality it needs to be redefined using the “data” mechanism, which requires introducing a dummy constructor (called `S` for brevity):

\begin{code}
data ST0 a = S0 (State -> (a, State))
\end{code}

It is convenient to define our own application function for this type, which simply removes the dummy constructor:

\begin{code}
apply0        :: ST0 a -> State -> (a, State)
apply0 (S0 f) x = f x
\end{code}

In turn, `ST` is now defined as a monadic type as follows:

\begin{code}
instance Monad ST0 where
  -- return :: a -> ST a
  return x   = S0 (\s -> (x,s))

  -- (>>=)  :: ST a -> (a -> ST b) -> ST b
  st >>= f   = S0 (\s -> let (x, s') = apply0 st s in apply0 (f x) s')
\end{code}

(Aside: the runtime overhead of manipulating the dummy constructor `S` can be eliminated by defining `ST` using the newtype mechanism of Haskell, rather than the data mechanism.)

A SIMPLE EXAMPLE
-----------------

Intuitively, a value of type `ST` a (or `ST0 a`) is simply an action that returns an a value. The sequencing combinators allow us to combine simple actions to get bigger actions, and the `apply0` allows us to execute an action from some initial state.

To get warmed up with the state-transformer monad, consider the simple sequencing combinator

```
(>>) :: Monad m => m a -> m b -> m b
```

in a nutshell, `a1 >> a2` takes the actions `a1` and `a2` and returns the mega action which is `a1`-then-`a2`-returning-the-value-returned-by-`a2`.

In other words, the function can be defined using the notion of a state transformer, in which the internal state is simply the next fresh integer

\begin{code}
type State = Int
\end{code}

In order to generate a fresh integer, we define a special state transformer that simply returns the current state as its result, and the next integer as the new state:

\begin{code}
fresh :: ST0 Int
fresh =  S0 (\n -> (n, n+1))
\end{code}

Note that `fresh` is a state transformer (where the state is itself just `Int`), that is an action that happens to return integer values. What do you think the following does:

\begin{code}
what1 = fresh >> 
        fresh >> 
        fresh >> 
        fresh
\end{code}

Exercise: What do you think this would return:

```
ghci> apply0 wtf1 0
```

Indeed, we are just chaining together four fresh actions to get a single action that “bumps up” the counter by 4.

Now, the `>>=` sequencer is kind of like `>>` only it allows you to “remember” intermediate values that may have been returned. Similarly,

```
return :: a -> ST0 a
```

takes a value `x` and yields an action that doesnt actually transform the state, but just returns the same value `x`. So, putting things together, how do you think this behaves?

\begin{code}
what2 = fresh >>= \n1 ->
        fresh >>= \n2 ->  
        fresh >>
        fresh >>
        return [n1, n2]
\end{code}

Eercise: What do you think this would return:

```
ghci> apply0 wtf2 0
```

Now, the `do` business is just nice syntax for the above:


\begin{code}
what3 = do n1 <- fresh
           fresh
           fresh
           fresh
           return n1
\end{code} 

is just like `what2`.

A MORE INTERESTING EXAMPLE
--------------------------

By way of an example of using the state monad, let us define a type of binary trees whose leaves contains values of some type a:

\begin{code}
data Tree a = Leaf a 
            | Node (Tree a) (Tree a)
            deriving (Eq, Show)
\end{code}

Here is a simple example:

\begin{code}
tree :: Tree Char
tree =  Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')
\end{code}

Now consider the problem of defining a function that labels each leaf in such a tree with a unique or “fresh” integer. This can be achieved by taking the next fresh integer as an additional argument to the function, and returning the next fresh integer as an additional result. In short, we can use

```
fresh :: ST0 Int
```

to get distinct integer values, together with the `return` and `>>=`  primitives that are provided by virtue of `ST` being a monadic type. It is now straightforward to define a function that takes a tree as its argument, and returns a state transformer that produces the same tree with each leaf labelled by a fresh integer:

\begin{code}
mlabel            :: Tree a -> ST0 (Tree (a,Int))
mlabel (Leaf x)   =  do n <- fresh
                        return (Leaf (x,n))
mlabel (Node l r) =  do l' <- mlabel l
                        r' <- mlabel r
                        return (Node l' r')
\end{code}

Note that the programmer does not have to worry about the tedious and error-prone task of dealing with the plumbing of fresh labels, as this is handled automatically by the state monad.

Finally, we can now define a function that labels a tree by simply applying the resulting state transformer with zero as the initial state, and then discarding the final state:

\begin{code}
label  :: Tree a -> Tree (a, Int)
label t = fst (apply0 (mlabel t) 0)
\end{code}

For example, label tree gives the following result:

```
ghci> label tree
Node (Node (Leaf ('a', 0)) (Leaf ('b',1))) (Leaf ('c', 2))
```

Exercise:
Define a function `app :: (State -> State) -> ST0 State`, 
such that fresh can be redefined by `fresh = app (+1)`.

Define a function `run :: ST0 a -> State -> a`,
such that label can be redefined by `label t = run (mlabel t) 0`.



A Generic State Transformer
----------------------------

Often, the state that we want to have will have multiple components, eg multiple variables whose values we might want to update. This is easily accomplished by using a different type for `State` above, for example, if we want two integers, we might use the definition

```
type State = (Int, Int)
```

and so on.

Since state is a handy thing to have, the standard library includes a module `Control.Monad.State` that defines a parameterized version of the state-transformer monad above. 

We will only allow clients to use the functions declared below

```
module MyState (ST, get, put, apply) where
```

The type definition for a generic state transformer is very simple:

\begin{code}
data ST s a = S (s -> (a, s))
\end{code}

is a parameterized state-transformer monad where the state is denoted by type `s` and the return value of the transformer is the type `a`. We make the above a monad by declaring it to be an instance of the monad typeclass

\begin{code}
instance Monad (ST s) where
  return x   = S (\s -> (x, s))
  st >>= f   = S (\s -> let (x, s') = apply st s 
                        in apply (f x) s')
\end{code}

where the function apply is just

\begin{code}
apply :: ST s a -> s -> (a, s)
apply (S f) x = f x
\end{code}

ACCESSING AND MODIFYING STATE
------------------------------

Since our notion of state is generic, it is useful to write a `get` and `put` function with which one can access and modify the state. We can easily get the current state via

\begin{code}
get = S (\s -> (s, s))
\end{code}

That is, `get` denotes an action that leaves the state unchanged, but returns the state itself as a value. 

What do you think the type of get is?

Dually, to modify the state to some new value `s'` we can write the function

\begin{code}
put s' = S (\_ -> ((), s'))
\end{code}

which denotes an action that ignores (ie blows away the old state) and replaces it with `s'`. Note that the `put s'` is an action that itselds yields nothing (that is, merely the unit value.)

Using a Generic State Transformer
---------------------------------

Let us use our generic state monad to rewrite the tree labeling function from above. Note that the actual type definition of the generic transformer is hidden from us, so we must use only the publicly exported functions: `get`, `put` and `apply` (in addition to the monadic functions we get for free.)

Recall the action that returns the next fresh integer. Using the generic state-transformer, we write it as:

\begin{code}
freshS :: ST Int Int
freshS = do n <- get
            put (n+1)
            return n
\end{code}

Now, the labeling function is straightforward

\begin{code}
mlabelS :: Tree a -> ST Int (Tree (a,Int))
mlabelS (Leaf x)   =  do n <- freshS
                         return (Leaf (x, n))
mlabelS (Node l r) =  do l' <- mlabelS l
                         r' <- mlabelS r
                         return (Node l' r')
\end{code}

Easy enough!

```
apply (mlabelS tree) 0
 == (Node (Node (Leaf ('a', 0)) (Leaf ('b', 1))) (Leaf ('c', 2)), 3)
```
We can execute the action from any initial state of our choice

```
apply (mlabelS tree) 1000
  == (Node (Node (Leaf ('a',1000)) (Leaf ('b',1001))) (Leaf ('c',1002)),1003)
```

Now, whats the point of a generic state transformer if we can’t have richer states. Next, let us extend our `fresh` and `label` functions so that

- each node gets a new label (as before),
- the state also contains a map of the frequency with which each leaf value appears in the tree.

Thus, our state will now have two elements, an integer denoting the next fresh integer, and a `Map a Int` denoting the number of times each leaf value appears in the tree.

\begin{code}
data MySt a = M { index :: Int
                , freq  :: Map a Int }
              deriving (Eq, Show)
\end{code}

We write an action that returns the next fresh integer as

\begin{code}
freshM = do 
  s     <- get              
  let n  = index s
  put $ s { index = n + 1 }  
  return n
\end{code} 

Similarly, we want an action that updates the frequency of a given element `k`

\begin{code}
updFreqM k = do 
  s    <- get               
  let f = freq s 
  let n = findWithDefault 0 k f
  put $ s {freq = insert k (n + 1) f}
\end{code}

And with these two, we are done

\begin{code}
mlabelM (Leaf x)   =  do updFreqM x 
                         n <- freshM
                         return $ Leaf (x, n)

mlabelM (Node l r) =  do l' <- mlabelM l
                         r' <- mlabelM r
                         return $ Node l' r'
\end{code}

Now, our initial state will be something like

\begin{code}
initM = M 0 empty
\end{code}

and so we can label the tree

```
tree2   = Node tree tree 
(lt, s) = apply (mlabelM tree) $ M 0 empty 
```

```
lt == 
Node (Node (Node (Leaf ('a',0)) (Leaf ('b',1))) (Leaf ('c',2))) (Node (Node (Leaf ('a',3)) (Leaf ('b',4))) (Leaf ('c',5)))
```

```
s == 
M {index = 6, freq = fromList [('a',2),('b',2),('c',2)]}
```

The IO Monad
------------

Interactive programs in Haskell are written using the type `IO a` of “actions” that return a result of type `a`, but may also perform some input/output. 
The `IO` is a state transformer where the state has the form of the outside word, 
i.e., the word that consists of the user input/output, the files in your system, the network, etc.
A number of primitives are provided for building values of this type, including:

```
return  :: a -> IO a
(>>=)   :: IO a -> (a -> IO b) -> IO b
getChar :: IO Char
putChar :: Char -> IO ()
```

The use of `return` and `>>=` means that `IO` is monadic, and hence that the `do` notation can be used to write interactive programs. For example, the action that reads a string of characters from the keyboard can be defined as follows:

```
getLine :: IO String
getLine =  do x <- getChar
              if x == '\n' then
                 return []
              else
                 do xs <- getLine
                    return (x:xs)
```

It is interesting to note that the `IO` monad can be viewed as a special case of the state monad, in which the internal state is a suitable representation of the “state of the world”:

```
   type World = ...

   type IO a  = World -> (a, World)
```

That is, an action can be viewed as a function that takes the current state of the world as its argument, and produces a value and a modified world as its result, in which the modified world reflects any input/output performed by the action. In reality, Haskell systems such as Hugs and GHC implement actions in a more efficient manner, but for the purposes of understanding the behaviour of actions, the above interpretation can be useful.

Derived primitives
------------------

An important benefit of abstracting out the notion of a monad into a single typeclass, is that it then becomes possible to define a number of useful functions that work in an arbitrary monad.

For example, the map function on lists can be generalised as follows:

\begin{code}
liftM     :: Monad m => (a -> b) -> m a -> m b
liftM f mx = do x <- mx
                return (f x)
\end{code}

Similarly, concat on lists generalises to:

\begin{code}
join    :: Monad m => m (m a) -> m a
join mmx = do mx <- mmx
              x  <- mx
              return x
\end{code}

As a final example, we can define a function that transforms a list of monadic expressions into a single such expression that returns a list of results, by performing each of the argument expressions in sequence and collecting their results:

```
sequence          :: Monad m => [m a] -> m [a]
sequence []       =  return []
sequence (mx:mxs) =  do x  <- mx
                        xs <- sequence mxs
                        return (x:xs)
```

MONADS AS PROGRAMMABLE SEMICOLON
---------------------------------
It is sometimes useful to sequence two monadic expressions, but discard the result value produced by the first:

```
(>>)     :: Monad m => m a -> m b -> m b
mx >> my =  do _ <- mx
               y <- my
               return y
```

For example, in the state monad the `>>` operator is just normal sequential composition, written as `;` in most languages.

Indeed, in Haskell the entire do notation with or without `;` is just syntactic sugar for `>>=` and `>>`. For this reason, we can legitimately say that Haskell has a programmable semicolon.

Exercise

- Define `liftM` and `join` more compactly by using `>>=`.

- Explain the behaviour of sequence for the maybe monad.

- Define another monadic generalisation of map:

```
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
```

- Define a monadic generalisation of foldr:

```
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
```

The monad laws
---------------

The notion of a monad requires that the `return` and `>>=` functions satisfy some simple properties. The first two properties concern the link between `return` and `>>=`:

```
return x >>= f  =  f x	--	(1)

mx >>= return   =  mx	--	(2)
```

Intuitively, equation (1) states that if we return a value `x` and then feed this value into a function `f`, this should give the same result as simply applying `f` to `x`. 
Dually, equation (2) states that if we feed the results of a computation `mx` into the function `return`, this should give the same result as simply performing `mx`. 
Together, these equations express — modulo the fact that the second argument to `>>=` involves a binding operation — that return is the left and right identity for `>>=`.

The third property concerns the link between `>>=` and itself, and expresses (again modulo binding) that `>>=` is associative:

```
(mx >>= f) >>= g  =  mx >>= (\x -> (f x >>= g)) 	-- (3)
```

Note that we cannot simply write `mx >>= (f >>= g)` on the right hand side of this equation, as this would not be type correct.

As an example of the utility of the monad laws, let us see how they can be used to prove a useful property of the `liftM` function from the previous section, namely that it distributes over the composition operator for functions, in the sense that:

```
liftM (f . g)  =  liftM f . liftM g
```

This equation generalises the familiar distribution property of map from lists to an arbitrary monad. In order to verify this equation, we first rewrite the definition of `liftM` using `>>=`:

```
liftM f mx  =  mx >>= \x -> return (f x)
```

Now the distribution property can be verified as follows:

```
(liftM f . liftM g) mx
   = {-   applying . -}
     liftM f (liftM g mx)
   = {-   applying the second liftM -}
     liftM f (mx >>= \x -> return (g x))
   = {-   applying liftM -} 
     (mx >>= \x -> return (g x)) >>= \y -> return (f y)
   = {-   equation (3) -}
     mx >>= (\z -> (return (g z) >>= \y -> return (f y)))
   = {-   equation (1) -}
     mx >>= (\z -> return (f (g z)))
   = {-   unapplying . -}
     mx >>= (\z -> return ((f . g) z)))
   = {-   unapplying liftM -}
     liftM (f . g) mx
```

Exercise:
- Show that the maybe monad satisfies equations `(1)`, `(2)` and `(3)`.


- Given the type

\begin{code}
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
\end{code}

of expressions built from variables of type `a`, show that this type is monadic by completing the following declaration:

```
   instance Monad Expr where
      -- return       :: a -> Expr a
      return x         = ...

      -- (>>=)        :: Expr a -> (a -> Expr b) -> Expr b
      (Var a)   >>= f  = ...
      (Val n)   >>= f  = ...
      (Add x y) >>= f  = ...
```

Hint: think carefully about the types involved. With the aid of an example, explain what the `>>=` operator for this type does.

OTHER TOPICS
------------
The subject of monads is a large one, and we have only scratched the surface here. If you are interested in finding out more, two suggestions for further reading would be to look at “monads with a zero a plus” (which extend the basic notion with two extra primitives that are supported by some monads), and “monad transformers” (which provide a means to combine monads.) For example, see sections 3 and 7 of the following article, which concerns the monadic nature of [functional parsers](http://www.cs.nott.ac.uk/~gmh/monparsing.pdf).
 For a more in-depth exploration of the `IO` monad, see Simon Peyton Jones’ excellent article on the [“awkward squad”](http://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/).
