data Maybe a = Nothing | Just a


data Perphams a = P a Prop



class Monad m where
	return :: a -> m a 
	bind :: m a -> (a -> m b) -> m b

class Functor m where 
	fmap :: (a -> b) ->  m a -> m b


{-
foo :: [Int]
foo = do x <- [1, 2]            bind [1, 2] (\x->
         y <- [3, 4]            bind [3, 4] (\y -> 
         return $ x + y         [x + y]))
-}




instance Monad Maybe where
	return x = Just x 
	bind Nothing _ = Nothing
	bind (Just x) f = Just (f x) 




instance Monad [] where
	return :: a -> [a]
	return x = [x]
	bind :: [a] -> (a -> [b]) -> [b]
	bind [] f = []
	bind (x:xs) f = f x ++ bind xs f 

instance Functor [] where
-- 	fmap :: (a -> b) -> [a] -> [b] 
	fmap f [] = []
	fmap f (x:xs) = f x : fmap f xs  


instance Functor Perhaps where
  fmap f (Perhaps x p) = Perhaps (f x) p
