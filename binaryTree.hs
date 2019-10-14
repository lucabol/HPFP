data BinaryTree a = Leaf
                    | Node (BinaryTree a) a (BinaryTree a)
                    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
             Nothing          -> Leaf
             Just (x1, y, x2) -> Node (unfold f x1) y (unfold f x2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x == n then Nothing else Just (x+1, x, x+1)) 0