module Chapter11 where


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

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = concat [[a], preorder left, preorder right]

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = concat [inorder left, [a], inorder right]

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = concat [postorder left, postorder right, [a]]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3] then putStrLn "Preorder fine!" else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3] then putStrLn "Inorder fine!" else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2] then putStrLn "Postorder fine!" else putStrLn "postorder failed check"
