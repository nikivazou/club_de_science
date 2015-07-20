Into Monads
-----------

\begin{code}
module Monads where
\end{code}

Division - Nothing example
--------------------------

A simple evaluator

\begin{code}
data Expr = Val Int | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val n)     = n
eval1 (Div e1 e2) = eval1 e1 `div` eval1 e2
\end{code}



< > eval1 (Div (Val 5) (Val 0))
< *** Exception: divide by zero