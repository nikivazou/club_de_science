\begin{code}
module Lambda where

import Debug.Trace (trace) 
import Data.List   (sort)

type VarId = String

data Expr 
  = EVar VarId
  | ELam VarId Expr
  | EApp Expr  Expr 
  deriving (Eq)

instance Show Expr where
	show (EVar x)     = x
	show (ELam x e)   = "λ" ++ x ++ "." ++ show e 
	show (EApp e1 e2) = showp e1 ++ " " ++ showp e2 

showp e@(EVar _) = show e
showp e = "(" ++ show e ++ ")" 

eid = ELam "x" $ EVar "x"
\end{code}

Booleans
--------
Functions that take two operants and return one

\begin{code}
etrue       = ELam "x1" $ ELam "y1" $ EVar "x1"
efalse      = ELam "x2" $ ELam "y2" $ EVar "y2"
eif b e1 e2 = EApp (EApp b e1) e2 
\end{code}


-- Quiz!
Not 

-- not true = false
-- not false = true

in Lambda Calculus


not (\xy.x) = \xy.y
not (\xy.y) = \xy.x


Fill in the dots
not = \b \x y. (.....)



\begin{code}
enot = ELam "b3" $ ELam "x3" $ ELam "y3" $ EApp (EApp (EVar "b3") (EVar "y3")) (EVar "x3")
\end{code}


\begin{code}
eor = ELam "b14" $ ELam "b24" $ ELam "x4" $ ELam "y4" $ 
      eif (EVar "b14") (EVar "x4") (eif (EVar "b24") (EVar "x4") (EVar "y4"))
\end{code}



Pairs
-----

\begin{code}

\end{code}

\begin{code}
eval ee@(EApp (ELam x e) ex) 
  = eval $ checkUniqueVars ex e $ subst (x, eval ex) e
eval (ELam x e)              
  = ELam x e 
eval (EApp e1 e2)            
  = eval $ EApp (eval e1) (eval e2) -- (eval e1) (eval e2) 
eval (EVar x)                
  = EVar x 


checkUniqueVars e1 e2 = checkDisjoint (boundVars e1) (boundVars e2)

checkDisjoint e1 e2 e = go (sort e1) (sort e2)
  where go (x:xs) (y:ys) | x == y = error $ "not disjoint" ++ show x
                         | x < y  = go xs (y:ys)
                         | y < x  = go (x:xs) ys
        go _       _              = e

boundVars (ELam x e)   = x:boundVars e
boundVars (EVar _)     = []
boundVars (EApp e1 e2) = boundVars e1 ++ boundVars e2

subst (x, ex) e@(EVar y)
  | x == y    = ex
  | otherwise = e
subst su@(x, ex) e@(ELam y ey)
  | x == y    = e
  | otherwise = ELam y (subst su ey)
subst su (EApp e1 e2)
              = EApp (subst su e1) (subst su e2) 
\end{code}

