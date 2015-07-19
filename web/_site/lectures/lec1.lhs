\begin{code}
module Intro where
\end{code}


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