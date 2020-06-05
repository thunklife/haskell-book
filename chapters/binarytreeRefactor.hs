module BinaryTreeRefactor where


data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

createTree :: a -> BinaryTree a
createTree a = Node Leaf a Leaf

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a(insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)


data Order = Preorder | Postorder | Inorder

order :: Order -> BinaryTree a -> [a]
order _ Leaf = []
order Preorder (Node left a right) = concat [[a], order Preorder left, order Preorder right]
order Postorder (Node left a right) = concat [order Postorder left, order Postorder right, [a]]
order Inorder (Node left a right) = concat [order Inorder left, [a], order Inorder right]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if order Preorder testTree == [2, 1, 3] then putStrLn "Preorder fine!" else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if order Postorder testTree == [1, 3, 2] then putStrLn "Postorder fine!" else putStrLn "postorder failed check"

testInorder :: IO ()
testInorder = if order Inorder testTree == [1, 2, 3] then putStrLn "Inorder fine!" else putStrLn "Bad news bears."

