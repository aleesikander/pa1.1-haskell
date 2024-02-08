
import Test.Hspec 

-- Use the following data types for the questions below
data Tree a = Nil | TreeNode (Tree a) a (Tree a) deriving (Show, Eq)

data LinkedList a = Null | ListNode a (LinkedList a) deriving (Show, Eq) 

--Category: Easy

-- Question 1
targetSum :: [Int] -> Int -> [[Int]]
targetSum [] _ = []
targetSum lst target = sortPairs $ myFilter (\[x, y] -> x >= y) $ uniquePairs lst target

uniquePairs :: [Int] -> Int -> [[Int]]
uniquePairs [] _ = []
uniquePairs (x:xs) target =
  [if x > y then [x, y] else [y, x] | y <- xs, x + y == target] ++ uniquePairs xs target

sortPairs :: [[Int]] -> [[Int]]
sortPairs [] = []
sortPairs (x:xs) = insertSorted x (sortPairs xs)

insertSorted :: [Int] -> [[Int]] -> [[Int]]
insertSorted x [] = [x]
insertSorted x@(x1:_) ys@(y@(y1:_):ys')
  | x1 < y1 || (x1 == y1 && (length x > 1 && length y > 1 && x !! 1 <= y !! 1)) = x : ys
  | otherwise = y : insertSorted x ys'

myFilter :: ([Int] -> Bool) -> [[Int]] -> [[Int]]
myFilter _ [] = []
myFilter pred (x:xs)
  | pred x    = x : myFilter pred xs
  | otherwise = myFilter pred xs


-- Question 2
symmetricTree :: Eq a => Tree a -> Bool
symmetricTree Nil = True
symmetricTree(TreeNode left _ right) = isMirror left right
  where
    isMirror :: Eq a => Tree a -> Tree a -> Bool
    isMirror Nil Nil = True
    isMirror (TreeNode l1 v1 r1) (TreeNode l2 v2 r2) = v1 == v2 && isMirror l1 r2 && isMirror r1 l2
    isMirror _ _ = False

-- Question 3
palindromList :: Eq a => LinkedList a -> Bool
palindromList list = let regularList = toList list
                    in regularList == reverse regularList
  where
    toList :: LinkedList a -> [a]
    toList Null = []
    toList (ListNode x xs) = x : toList xs

-- Question 4
appendTo :: [a] -> a -> [a]
appendTo [] x = [x]
appendTo (y:ys) x = y : appendTo ys x

-- Function to reverse a list
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = appendTo (reverseList xs) x

-- Get the next level of nodes
nextLevel :: [Tree a] -> [Tree a]
nextLevel [] = []
nextLevel (Nil:xs) = nextLevel xs
nextLevel (TreeNode l _ r:xs) = l : r : nextLevel xs

-- Get the values of the current level
valuesAtLevel :: [Tree a] -> [a]
valuesAtLevel [] = []
valuesAtLevel (Nil:xs) = valuesAtLevel xs
valuesAtLevel (TreeNode _ a _:xs) = a : valuesAtLevel xs

-- Function to perform snake traversal on a tree
snakeTraversal :: Tree a -> [a]
snakeTraversal Nil = []
snakeTraversal tree = traverse [tree] True
  where
    traverse [] _ = []
    traverse level ltr = 
      let vals = valuesAtLevel level
          valsInOrder = if ltr then vals else reverseList vals
      in valsInOrder ++ traverse (nextLevel level) (not ltr)


-- Question 5
treeConstruction :: String -> Tree Char
treeConstruction str = fst $ buildTree str Nil where
  buildTree :: String -> Tree Char -> (Tree Char, String)
  buildTree [] tree = (tree, [])
  buildTree ('^':xs) tree = (tree, xs)
  buildTree (x:xs) Nil = buildTree xs (TreeNode Nil x Nil)
  buildTree (x:xs) (TreeNode left val right)
    | left == Nil = let (newLeft, rest) = buildTree xs (TreeNode Nil x Nil)
                    in buildTree rest (TreeNode newLeft val right)
    | otherwise   = let (newRight, rest) = buildTree xs (TreeNode Nil x Nil)
                    in (TreeNode left val newRight, rest)



-- Category: Medium

-- Attempy any 4 questions from this category

-- Question 1.1: Overload the (+) operator for Tree. You only need to overload (+). Keep the rest of the operators as undefined.   
instance Num a => Num (Tree a) where
  (+) = treeAdd
  -- These instances are needed to satisfy Num but are not meaningful for Tree.
  (-) = \_ _ -> Nil
  (*) = \_ _ -> Nil
  abs = const Nil
  signum = const Nil
  fromInteger = const Nil

-- Define how two trees are added together
treeAdd :: Num a => Tree a -> Tree a -> Tree a
treeAdd Nil b = b
treeAdd a Nil = a
treeAdd (TreeNode aLeft aVal aRight) (TreeNode bLeft bVal bRight) =
  TreeNode (treeAdd aLeft bLeft) (aVal + bVal) (treeAdd aRight bRight)


-- Question 1.2

longestCommonString :: Eq a => LinkedList a -> LinkedList a -> LinkedList a
longestCommonString l1 l2 = maximumByLength [commonSubstring a b | a <- tails l1, b <- tails l2]
  where
    -- Find the common substring from the start of two lists
    commonSubstring :: Eq a => LinkedList a -> LinkedList a -> LinkedList a
    commonSubstring Null _ = Null
    commonSubstring _ Null = Null
    commonSubstring (ListNode x xs) (ListNode y ys)
        | x == y = ListNode x (commonSubstring xs ys)
        | otherwise = Null

    -- Generate all tails of a list (all possible starting points for substrings)
    tails :: LinkedList a -> [LinkedList a]
    tails Null = [Null]
    tails l@(ListNode _ xs) = l : tails xs

    -- Select the maximum length list from a list of lists
    maximumByLength :: [LinkedList a] -> LinkedList a
    maximumByLength [] = Null
    maximumByLength (x:xs) = foldl (\acc y -> if listLength y > listLength acc then y else acc) x xs

    -- Calculate the length of a LinkedList
    listLength :: LinkedList a -> Int
    listLength Null = 0
    listLength (ListNode _ xs) = 1 + listLength xs

-- Question 2
existsInTree :: (Ord a) => Tree a -> a -> Bool
existsInTree Nil _ = False
existsInTree (TreeNode left val right) x
  | x == val = True
  | x < val = existsInTree left x
  | otherwise = existsInTree right x

commonAncestor :: (Ord a) => Tree a -> a -> a -> Maybe a
commonAncestor Nil _ _ = Nothing
commonAncestor tree@(TreeNode left val right) p q
  | not (existsInTree tree p && existsInTree tree q) = Nothing
  | val == p || val == q = Just val
  | p < val && q < val = commonAncestor left p q
  | p > val && q > val = commonAncestor right p q
  | otherwise = Just val

-- Question 3
gameofLife :: [[Int]] -> [[Int]]
gameofLife = undefined

-- Question 4
waterCollection :: [Int] -> Int
waterCollection heights = (*2) $ sum $ map calculateWater [1 .. length heights - 2]
  where
    calculateWater i = let
      leftMax = maximum $ take i heights
      rightMax = maximum $ drop (i + 1) heights
      in max 0 (min leftMax rightMax - (heights !! i))


-- Question 5
minPathMaze :: [[Int]] -> Int
minPathMaze = undefined





-- Main Function
main :: IO ()
main =
   hspec $ do

    -- Test List Target Sum
        describe "targetSum" $ do
            it "should return pairs whose sum is equal to the target" $ do
                targetSum [1,2,3,4,5] 5 `shouldBe` [[3,2], [4,1]]
                targetSum [1,2,3,4,5,6] 10 `shouldBe` [[6,4]]
                targetSum [1,2,3,4,5] 0 `shouldBe` []
                targetSum [1,10,8,7,6,2,3,4,5,-1,9] 10 `shouldBe` [[6,4],[7,3],[8,2],[9,1]]
    
    -- Test Symmetric Tree
        describe "symmetricTree" $ do
            it "should return True if the tree is symmetric" $ do
                symmetricTree (Nil :: Tree Int) `shouldBe` True
                symmetricTree (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 1 Nil)) `shouldBe` True
                symmetricTree (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 2 Nil)) `shouldBe` False
                symmetricTree (TreeNode (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 4 (TreeNode (TreeNode Nil 3 Nil) 2 (TreeNode Nil 1 Nil))) `shouldBe` True
                symmetricTree (TreeNode (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 4 (TreeNode (TreeNode Nil 3 Nil) 2 (TreeNode Nil 4 Nil))) `shouldBe` False
    
    -- Test Palindrom List
        describe "palindromList" $ do
            it "should return True if the list is a palindrome" $ do
                palindromList (Null :: LinkedList Int) `shouldBe` True
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 1 Null))))) `shouldBe` True
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 3 (ListNode 1 Null))))) `shouldBe` False
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 2 Null))))) `shouldBe` False
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 1 (ListNode 1 Null)))))) `shouldBe` False
                palindromList (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'b' (ListNode 'a' Null))))) `shouldBe` True
                palindromList (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'c' (ListNode 'a' Null))))) `shouldBe` False
    
    -- Test Snake Traversal
        describe "snakeTraversal" $ do
            it "should return the snake traversal of the tree" $ do
                snakeTraversal (Nil:: Tree Int) `shouldBe` []
                snakeTraversal (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) `shouldBe` [2,3,1]
                snakeTraversal (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 7 Nil))) `shouldBe` [4,2,3,1,6,5,7]
                snakeTraversal (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode (TreeNode Nil 9 Nil) 7 Nil))) `shouldBe` [4,2,3,1,6,5,7,9]
    
    -- Test Tree Construction
        describe "treeConstruction" $ do
            it "should return the tree constructed from the string" $ do
                treeConstruction "" `shouldBe` Nil
                treeConstruction "a" `shouldBe` TreeNode Nil 'a' Nil
                treeConstruction "^a" `shouldBe` Nil
                treeConstruction "ab^c" `shouldBe` TreeNode (TreeNode Nil 'b' Nil) 'a' (TreeNode Nil 'c' Nil)
                treeConstruction "ab^c^" `shouldBe` TreeNode (TreeNode Nil 'b' Nil) 'a' (TreeNode Nil 'c' Nil)
                treeConstruction "ab^cde^f" `shouldBe` TreeNode (TreeNode Nil 'b' Nil) 'a' (TreeNode (TreeNode (TreeNode Nil 'e' Nil) 'd' (TreeNode Nil 'f' Nil)) 'c' Nil)
                treeConstruction "abcde^f" `shouldBe` TreeNode (TreeNode (TreeNode (TreeNode (TreeNode Nil 'e' Nil) 'd' (TreeNode Nil 'f' Nil)) 'c' Nil) 'b' Nil) 'a' Nil
    
    -- Test (+) operator for Tree
        describe "(+)" $ do
            it "should return the sum of the two trees" $ do
                let result1 = (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil) + TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil) :: Tree Int) 
                result1  `shouldBe` TreeNode (TreeNode Nil 2 Nil) 4 (TreeNode Nil 6 Nil) 
                let result2 = (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil) + TreeNode Nil 2 (TreeNode Nil 3 Nil) :: Tree Int)
                result2 `shouldBe` TreeNode (TreeNode Nil 1 Nil) 4 (TreeNode Nil 6 Nil)
                let result3 = (Nil + Nil :: Tree Int) 
                result3 `shouldBe` Nil
                let result4 = (Nil + TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil):: Tree Int)
                result4 `shouldBe` TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)
                let result5 = (TreeNode (TreeNode (TreeNode Nil 1 (TreeNode Nil (-2) Nil)) 3 Nil) 4 (TreeNode Nil 2 (TreeNode Nil 7 (TreeNode Nil (-7) Nil))) + TreeNode (TreeNode (TreeNode (TreeNode Nil 0 Nil) 1 Nil) 3 (TreeNode (TreeNode Nil 1 Nil) 6 (TreeNode Nil (-2) Nil))) 4 (TreeNode (TreeNode (TreeNode Nil 9 Nil) 5 (TreeNode Nil 4 Nil)) 2 (TreeNode (TreeNode Nil (-5) Nil) 7 Nil)) :: Tree Int) 
                result5 `shouldBe` TreeNode (TreeNode (TreeNode (TreeNode Nil 0 Nil) 2 (TreeNode Nil (-2) Nil)) 6 (TreeNode (TreeNode Nil 1 Nil) 6 (TreeNode Nil (-2) Nil))) 8 (TreeNode (TreeNode (TreeNode Nil 9 Nil) 5 (TreeNode Nil 4 Nil)) 4 (TreeNode (TreeNode Nil (-5) Nil) 14 (TreeNode Nil (-7) Nil)))
                let result6 = (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 7 Nil)) + TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 7 Nil)) :: Tree Int) 
                result6 `shouldBe` TreeNode (TreeNode (TreeNode Nil 2 Nil) 6 (TreeNode Nil 12 Nil)) 8 (TreeNode (TreeNode Nil 10 Nil) 4 (TreeNode Nil 14 Nil))
    
    -- Test Longest Common String
        describe "longestCommonString" $ do
            it "should return the longest common string" $ do
                longestCommonString (Null::LinkedList Char) (Null::LinkedList Char) `shouldBe` Null
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) Null `shouldBe` Null
                longestCommonString Null (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) `shouldBe` Null
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'f' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' Null)))
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'f' (ListNode 'e' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' (ListNode 'c' Null))
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'f' (ListNode 'g' (ListNode 'e' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' Null)
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'f' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) `shouldBe` ListNode 'c' (ListNode 'd' (ListNode 'e' Null))
    
    -- Test Common Ancestor
        describe "commonAncestor" $ do
            it "should return the lowest common ancestor of the two nodes" $ do
                commonAncestor Nil 1 2 `shouldBe` Nothing
                commonAncestor (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 1 3 `shouldBe` Just 2
                commonAncestor (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 1 4 `shouldBe` Nothing
                commonAncestor (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 4 Nil)) 5 (TreeNode (TreeNode Nil 6 Nil) 8 (TreeNode Nil 9 Nil))) 1 6 `shouldBe` Just 5
                commonAncestor (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 4 Nil)) 5 (TreeNode (TreeNode Nil 6 Nil) 8 (TreeNode Nil 9 Nil))) 8 9 `shouldBe` Just 8
                commonAncestor (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 4 Nil)) 5 (TreeNode (TreeNode Nil 6 Nil) 8 (TreeNode Nil 9 Nil))) 1 3 `shouldBe` Just 3
                
    
    -- Test Game of Life
        describe "gameofLife" $ do
            it "should return the next state" $ do
                gameofLife [[0,1,0],[0,0,1],[1,1,1],[0,0,0]] `shouldBe` [[0,0,0],[1,0,1],[0,1,1],[0,1,0]]
                gameofLife [[1,1],[1,0]] `shouldBe` [[1,1],[1,1]]
                gameofLife [[1,1],[1,1]] `shouldBe` [[1,1],[1,1]]
                gameofLife [[1,0],[0,1]] `shouldBe` [[0,0],[0,0]]
                gameofLife [[0,1,0,0],[0,1,1,1],[1,0,1,1]] `shouldBe` [[0,1,0,0],[1,0,0, 1],[0,0,0,1]]
    
    -- Test Water Collection
        describe "waterCollection" $ do
            it "should return the amount of water that can be trapped" $ do
                waterCollection [0,1,0,2,1,0,1,3,2,1,2,1] `shouldBe` 12
                waterCollection [4,2,0,3,2,5] `shouldBe` 18
                waterCollection [1,2,3,4,5] `shouldBe` 0
                waterCollection [5,4,3,2,1] `shouldBe` 0
                waterCollection [5,4,3,2,1,2,3,4,5] `shouldBe` 32  
                waterCollection [1, 0, 2, 3, 1, 4] `shouldBe` 6
                waterCollection [0, 4, 1, 2, 0, 1, 3] `shouldBe` 16
    
    -- Test Min Path Maze
        describe "minPathMaze" $ do
            it "should return the minimum cost to reach the bottom right cell" $ do
                minPathMaze [[1,3,1],[1,5,1],[4,2,1]] `shouldBe` 7
                minPathMaze [[1,2,3],[4,5,6],[7,8,9]] `shouldBe` 21
                minPathMaze [[1,2,3,4],[4,5,6,7],[7,8,9,9],[10,11,1,13]] `shouldBe` 35
                minPathMaze [[1,2,3,4,5],[4,5,6,7,8],[7,8,9,9,10],[10,11,1,13,14],[15,16,17,18,19]] `shouldBe` 66
                minPathMaze [[1,2,3,4,5,6],[4,1,2,7,8,9],[7,8,1,2,10,11],[10,11,1,2,22,15],[15,16,17,1,2,20],[21,22,23,24,2,26]] `shouldBe` 41