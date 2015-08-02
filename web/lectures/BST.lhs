Binary Search Trees
====================


A _binary search tree_ (BST) is a binary tree where each node has a Comparable key (and an associated value) and satisfies the restriction that the key in any node is larger than the keys in all nodes in that node's left subtree and smaller than the keys in all nodes in that node's right subtree.

\begin{code}
module BST (
	Map,

	-- Construction 
	empty, singleton,

	-- Insertion
	insert, insertWith, 

	-- Deletion 
	delete,

	-- Searching
	find, findWithDefault

	) where
\end{code}


The data type
-------------
A Binary Search Tree is a Map from _keys_ of type `k` 
to _values_ of type _v_. 
The tree is 

* either empty 

* or contains a key `k` that maps to a value `v` and 
    - a left subtree, whose keys are less than `k`, 
    - a right subtree, whose keys are greater than `k`, and 
    - a size that is the maximum size of the subtrees increased by 1. 

\begin{code}
data Map k v = Tip 
             | Bin Size k v (Map k v) (Map k v)
             deriving (Show)

type Size = Int
\end{code}


Construction
-------------

How do we construct BST?
One way would be to use the data constuctors, e.g.,

\begin{code}
tree2 = Bin 1 2 "two"  Tip Tip
tree4 = Bin 1 4 "four" Tip Tip
tree3 = Bin 2 3 "three" tree2 tree4
\end{code}

- Exercise: Add an entry for the pair `(1, "one")` in the above three.


Annoying right? We need to create functions that manipulate these trees!


Write a function that returns the empty tree:

\begin{code}
empty :: Map k a
empty = undefined
\end{code}

Write a function that given a key and a value, 
it returns a Map that contains them

< singeton 1 "one" = Bin 1 1 "one" Tip Tip
< singeton 2 "two" = Bin 1 2 "two" Tip Tip

\begin{code}
singleton :: k -> a -> Map k a
singleton k x = Bin 1 k x Tip Tip
\end{code}


Write a function that returns the `size` of the tree

< size empty              == 0
< size (singleton 1 'a')  == 1
< size tree1              == 2

\begin{code}
size :: Map k a -> Int
size t = undefined
\end{code}


Insertion
----------
Write a function that inserts the key `k` and the value `x` to the tree `t`


< insert 4 "four" tree1 = Bin 2 3 "three" Tip (Bin 1 4 "four" Tip Tip)

\begin{code}
insert :: Ord k => k -> v -> Map k v -> Map k v 
insert = undefined
\end{code}

What is the _complexity_ of insert?
Can we do better?
_Yes_, if the trees are _balanced_!

A tree is balanced both the left and the right subtrees have about the same size!
Formally, we define the _balanceFactor_ as

< balanceFactor = size(left subtree) - size(right subtree)

A tree is balanced is the `balanceFactor` is between `-1` and `1`.

Insertion of a node in a balanced tree can modify the balanced factor by 1, 
leading to balanced factors between `-2` and `2`. 
Then, one of the following rotations may be requied:

<img src="https://upload.wikimedia.org/wikipedia/commons/f/f5/AVL_Tree_Rebalancing.svg" width="500"  />

- Left Left Case:
Let us first assume the balance factor of a node P is 2 (as opposed to the other possible unbalanced value −2). This case is depicted in the left column of the illustration with P:=5. We then look at the left subtree (the higher one) with root N. If this subtree does not lean to the right - i.e. N has balance factor 1 (or, when deletion also 0) - we can rotate the whole tree to the right to get a balanced tree. This is labelled as the "Left Left Case" in the illustration with N:=4. 

- Left Right Case:
If the subtree does lean to the right - i.e. N:=3 has balance factor −1 - we first rotate the subtree to the left and end up the previous case. This second case is labelled as "Left Right Case" in the illustration.

- Right Right Case:
If the balance factor of the node P is −2 (this case is depicted in the right column of the illustration P:=3) we can mirror the above algorithm. I.e. if the root N of the (higher) right subtree has balance factor −1 (or, when deletion also 0) we can rotate the whole tree to the left to get a balanced tree. This is labelled as the "Right Right Case" in the illustration with N:=4. 

- Right Left Case:
If the root N:=5 of the right subtree has balance factor 1 ("Right Left Case") we can rotate the subtree to the right to end up in the "Right Right Case".

Implement tha above _balancing_ algorithm:

\begin{code}
balance :: k -> v -> Map k v -> Map k v -> Map k v
balance k v l r = undefined
\end{code}

Use `balance` to improve the complexity of the `insert` function.

Testing Trees
-------------
We need to test if our inserting function works properly. 
To do so, first define a function `fromList` that turns a list into a Map

\begin{code}
fromList :: Ord k => [(k, v)] -> Map k v
fromList xs = undefined
\end{code}

and vice versa

\begin{code}
toList :: Map k v -> [(k, v)]
toList t = undefined
\end{code}


Then write the properties you want to check: 
ordering and balancing

\begin{code}
isOrdered :: Ord k => Map k v -> Bool
isOrdered Tip             
  = True 
isOrdered (Bin _ k v l r) 
  = isOrdered l && isOrdered r && all (k>) (keys l) && all (k<) (keys r)

keys :: Map k v -> [k]
keys = map fst . toList  


isBalanced :: Ord k => Map k v -> Bool
isBalanced = undefined
\end{code}

Then, come up with list of pairs, turns them to maps and test the properties on them

\begin{code}
bst1 = fromList $ [(1, "one"), (5, "five"), (6, "six"), (3, "three")]

check1 = isOrdered bst1 && isBalanced bst1 
\end{code}

What if the key you want to insert already exists in the tree?
Write a function `insertWith` that combines the old and the new values in the tree:

< insertWith (++) 1 "ten &" (fromList [(1, "one")]) = fromList [(1, "ten & one")]

\begin{code}
insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f k v t = undefined 
\end{code}

Deletion
--------
Write a function the deletes the key `k` from a tree

\begin{code}
delete :: Ord k => k -> Map k v -> Map k v
delete k t = undefined
\end{code}

Did you rebalance the tree?


Searching 
---------
`lookup k t` returns `Just v` if the binding `(k, v)` exists on `t`.
Otherwise, it returns `Nothing`

\begin{code}
find :: Ord k => k -> Map k a -> Maybe a
find k t = undefined
\end{code}

What is the complexity of `lookup`?

Instead of returning `Nothing` values, the next function
`findWithDefault` get one more "default" argument and if the 
key is not found in the tree it returns the default value!

\begin{code}
findWithDefault :: Ord k => v -> k -> Map k v -> v
findWithDefault = undefined
\end{code}