isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust _        = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing = x
mayybee _ f (Just k) = f k

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just y) = y

fromMaybeUnsafe :: Maybe a -> a
fromMaybeUnsafe Nothing = error "Nothing?"
fromMaybeUnsafe (Just y) = y

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_)  = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: Eq a => [Maybe a] -> [a]
catMaybes = map fromMaybeUnsafe . filter isJust

flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe l
    | Nothing `elem` l = Nothing
    | otherwise        = Just (map fromMaybeUnsafe l)

