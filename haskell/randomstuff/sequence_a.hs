import Control.Applicative

-- Let's try implementing a function that takes a list of applicatives and 
-- returns an applicative that has a list as its result value.

--sequenceA :: Applicative f => [f a] -> f [a]

sequenceA list = foldr (liftA2 (:)) (pure []) list

sequenceA' []     = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs
