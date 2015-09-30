-- exercises of chapter 03
import Data.List (sortBy)

-- 01, 02
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = tailLength xs 1
    where tailLength [] n = n
          tailLength (x:xs) n = tailLength xs (n+1)

-- 03
myMean [] = Nothing
myMean xs = Just (sum / length)
    where (sum, length) = tailMean xs 0.0 0.0
              where tailMean [] s n = (s, n)
                    tailMean (x:xs) s n = tailMean xs (s+x) (n+1)

-- 04
myPalindrom :: [a] -> [a]
myPalindrom xs = xs ++ (myReverse xs [])
    where myReverse [] ys = ys
          myReverse (x:xs) ys = myReverse xs (x:ys)

-- 05
isPalindrom :: Eq a => [a] -> Bool
isPalindrom xs = xs == reverse xs

-- 06
lengthSort :: [[a]] -> [[a]]
lengthSort xs = sortBy sortByLength xs
    where sortByLength xs ys = compare (length xs) (length ys)

-- 07
myJoin :: a -> [[a]] -> [a]
myJoin s [] = []
myJoin s (x:[]) = x
myJoin s (x:xs) = tailJoin xs x
    where tailJoin [] ys     = ys
          tailJoin (x:xs) ys = tailJoin xs (ys ++ [s] ++ x)

-- 08
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)
myHeight :: Tree a -> Int
myHeight Empty = 0
myHeight (Node a left right) = max (1 + myHeight left) (1 + myHeight right)

-- 09
data Direction = Straight
               | TurnLeft
               | TurnRight
               deriving (Show, Eq)

-- 10
type Point = (Double, Double)
direction :: Point -> Point -> Point -> Direction
direction (ax,ay) (bx,by) (cx,cy)
  | v1 == v2 = Straight
  | v1 > v2 = TurnLeft
  | v1 < v2 = TurnRight
      where v1 = (cy-by)*(bx-ax)
            v2 = (cx-bx)*(by-ay)

-- 11
mapDirection :: [Point] -> [Direction]
mapDirection (a:b:c:xs) = tailDirection [] (a:b:c:xs)
    where tailDirection ds (a:b:c:xs) = tailDirection ((direction a b c):ds) (b:c:xs)
          tailDirection ds _ = reverse ds
mapDirection _ = []

-- 12
grahamScan :: [Point] -> [Point]
grahamScan xs
    | length xs <= 2 = []
    | otherwise = tailScan [base] (sortBy (cmpAngleWithBase base) remain)
    where (base,remain) = findBase (head xs) [] xs
              where findBase p remain [] = (p,remain)
                    findBase p remain (x:xs)
                        | xy < py = findBase x (p:remain) xs
                        | xy == py && xx < py = findBase x (p:remain) xs
                        | otherwise = findBase p (x:remain) xs
                            where (px,py) = p
                                  (xx,xy) = x
          -- using cot(theta), larger ones with smaller angles, because
          -- tan(theta) can't preserve the ordering when signs change
          cmpAngleWithBase (px,py) a b
            | rkb /= rka = compare rkb rka
            | otherwise  = compare (fst a) (fst b)
                where rkb = slope b
                      rka = slope a
                      slope (x,y) = (x-px)/(y-py)
          tailScan (a:b:ys) (x:xs)
            | turn == TurnLeft = tailScan (x:a:b:ys) xs
            | turn == TurnRight = tailScan (b:ys) (x:xs)
            | turn == Straight = tailScan (x:b:ys) xs
              where turn = direction b a x
          tailScan ys [] = reverse ys
          -- length ys <= 1, copy head of xs to ys
          tailScan ys (x:xs) = tailScan (x:ys) xs
