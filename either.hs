-- much better than mine
lefts' :: [Either a b] -> [a]
lefts' x = [a | Left a <- x]

lefts'' :: [Either a b] -> [a]
lefts'' es = foldr go [] es where go (Right _) acc = acc
                                  go (Left x) acc = x:acc

rights' :: [Either a b] -> [b]
rights' es = foldr go [] es where go (Left _) acc = acc
                                  go (Right x) acc = x:acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' lf _ (Left l) = lf l
either' _ rf (Right r) = rf r

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e = either' (\_ -> Nothing) (\x -> Just (f x)) e

myIterate :: (a -> a) -> a -> [a]
myIterate f x = [x] ++ myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
        Nothing     -> []
        Just (x, y) -> [x] ++ myUnfoldr f y

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\y -> Just (y, f y))