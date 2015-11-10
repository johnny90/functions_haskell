import Data.List


addDigit :: Int -> Int -> Int
addDigit number c
 = number*10 + c

convert :: Float -> Float
convert temperature
 = temperature * 9 / 5 + 32

type Vertex = ( Float, Float )

distance :: Vertex -> Vertex -> Float
distance a b
 = sqrt( ( fst a - fst b ) ^ 2 + ( snd a - snd b ) ^ 2)

triArea :: Vertex -> Vertex -> Vertex -> Float
triArea x y z
 = sqrt ( s*(s-a)*(s-b)*(s-c) )
   where
        a = distance x y
        b = distance y z
        c = distance z x
        s = (a+b+c) / 2

fact :: Int -> Int
fact n
   | n==1 = 1
   | otherwise = n*fact(n-1)

perm :: Int -> Int -> Int
perm n r
   | n==r = 1
   | n>r = n*perm (n-1) r

choose :: Int -> Int -> Int
choose n r
   | n==r = 1
   | otherwise = n * choose (n-1) r `div` (n-r)

remainder :: Int -> Int -> Int
remainder a b
 = if(a<b) then a else remainder (a-b) b

binary :: Int -> Int
binary d
   | d<2 = d
   | otherwise = ( binary (div d 2) ) * 10 + d `mod` 2

chop :: Int -> (Int, Int)
chop n
 = if (n<10) then (0,n)
   else (1+q,r)
      where
         (q,r) = chop (n-10)

precedes :: String -> String -> Bool
precedes [] []         = True
precedes x []          = False
precedes [] x          = True
precedes (x:xs) (y:ys) = if (x==y) then precedes xs ys else x<y

pos :: Int -> [Int] -> Int
pos nr (x:xs) = if(nr==x) then 0 else (1+pos nr xs)

myLength :: [a] -> Int
myLength []     = 0
myLength (x:xs) = 1 + myLength xs

append :: [a] -> [a] -> [a]
append [] x     = x
append (x:xs) y = x : append xs y

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] x  = x
merge x []  = x
merge (x:xs) (y:ys)
  | x < y     = x:y:merge xs ys
  | otherwise = y:x:merge xs ys

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x]= [x]
mergeSort xs = merge (mergeSort xs') (mergeSort xs'')
  where
    xs'  = take nr xs
    xs'' = drop nr xs
    nr   = length xs `div` 2

twoSame :: [Int] -> Bool
twoSame [x] = False
twoSame (x:xs:xss)
  | x == xs   = True
  | otherwise = twoSame (xs:xss)

primeFactors :: Int -> [ Int ]
-- Pre: x >= 1
primeFactors number = primeFactors' number 2
  where
    primeFactors' :: Int -> Int -> [Int]
    primeFactors' x y
      | x==1           = []
      | x `mod` y == 0 = y : primeFactors' (x `div` y) y
      | otherwise      = primeFactors' x (y + (y `mod` 2) + 1) -- just to test odd numbers after 2

hcf :: Int -> Int -> Int
hcf a b
  | a < 1 || b < 1 = error "Numbers less than 1"
  | otherwise      = a `div` c
  where
    ps = primeFactors a
    ps'= primeFactors b
    c  = product (ps \\ ps')

lcom :: Int -> Int -> Int
lcom a b
  | a < 1 || b < 1 = error "Numbers less than 1"
  | otherwise      = b * c
  where
    ps = primeFactors a
    ps'= primeFactors b
    c  = product (ps \\ ps')

right :: a -> [a] -> [a]
right x []     = [x]
right x (y:ys) = y : right x ys

backwards :: [a] -> [a]
backwards []     = []
backwards (x:xs) = right x (backwards xs)

backwards' :: [a] -> [a] -> [a]
backwards' [] acc     = acc
backwards' (x:xs) acc = backwards' xs (x:acc)

merge2 :: [Int] -> [Int] -> [Int]
merge2 [] [] = []
merge2 [] x  = x
merge2 x  [] = x
merge2 l1@(x:xs) l2@(y:ys)
  | x < y     = x : merge2 xs l2
  | otherwise = y : merge2 l1 ys

makeTuples :: String -> String -> [(Char,Char)]
makeTuples [] [] = []
makeTuples (x:xs) (y:ys) = (x,y) : makeTuples xs ys

anagram :: String -> String -> String -> String
anagram _ _ [] = []
anagram x old (new:news)
  = (findReplace def new) : (anagram x old news)
  where
    def = makeTuples x old

findReplace :: [(Char,Char)] -> Char -> Char
findReplace [] c = ' '
findReplace (x:xs) c
  | c == snd x = fst x
  | otherwise  = findReplace xs c

substring :: String -> String -> Bool
substring s p = substring' s list  
  where
    list = genSuffix p

substring' :: String -> [String] -> Bool
substring' [] _ = True
substring' x [] = False
substring' x (y:ys)
  | x == take nr y = True
  | otherwise      = substring' x ys
  where
    nr = length x

genSuffix :: String -> [String]
genSuffix [] = []
genSuffix (x:xs) = xs : genSuffix xs

findAll x db = [y | (x,y) <- db]

timesTable :: Int -> Int -> [String]
timesTable a b = [show x ++ " times " ++ show y ++ " is " ++ show (x*y) | x <- [1..a], y <- [1..b]]

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort list
  = quicksort m ++ [f] ++ quicksort n
  where
    f = head list
    m = [x | x<-(tail list), x <= f]
    n = [y | y<-(tail list), y > f]

perms :: (Eq a) => [a] -> [[a]]
perms list = perms' list n [[r]|r<-list]
  where
    n = length list
    perms' :: (Eq a) => [a] -> Int -> [[a]] -> [[a]]
    perms' [] _ _ = []
    perms' l 1 acc = acc
    perms' l n acc = perms' l (n-1) accum
      where
	accum = [r:xs| r<-l, xs<-acc, (r:xs) \\ l == []]

subpops :: [Int] -> Int -> [[Int]]
subpops ps k
  = subpops' ps k (length ps) [[]]
  where
    subpops' :: [Int] -> Int -> Int -> [[Int]] -> [[Int]]
    subpops' _ _ 0 acc = (filter (\x-> sum x == k) acc)
    subpops' ps k n acc
      = subpops' ps k (n-1) acc'
      where
        acc' = [ (x:xs) | x <- [0,1..(head(drop (n-1) ps))], xs <- acc ]

mergeN :: [[Int]] -> [Int]
mergeN
  = foldr merge2 []

mergeN' :: [[Int]] -> [Int]
mergeN' []
  = []
mergeN' lists
  = minh : mergeN' (filter f (map (\\[minh]) lists))
  where
    minh = minimum (map head lists)
    f :: [a] -> Bool
    f x = (length x)/=0

same :: [Int] -> Bool
same xs
  = and (zipWith (\x y-> x==y) xs (tail xs))

squash :: (a -> a -> b) -> [a] -> [b]
squash f xs
  = zipWith f xs (tail xs)

squash' :: (a -> a -> b) -> [a] -> [b]
squash' f [x]
  = []
squash' f (x:(z@(xs:xss)))
  = (f x xs) : squash' f z

converge :: (a -> a -> Bool) -> [a] -> a
converge f xs
  | newList == [] = head xs
  | otherwise     = head (drop (length xs - length newList) xs)
  where
    newList = dropWhile (\x -> x==False) (squash f xs)

any', all' :: (a -> Bool) -> [a] -> Bool
any' = (or.).(map)
all' = (and.).(map)

data Shape = Triangle Float Float Float
           | Square Float
           | Circle Float
           | Polygon [(Float,Float)]

area :: Shape -> Float
area (Triangle a b c)
  = sqrt(s*(s-a)*(s-b)*(s-c))
  where
    s = (a+b+c) / 2
area (Square x)
  = x*x
area (Circle r)
  = pi * r * r
area (Polygon vertices)
  | vertices == [] = 0
  | otherwise      = area (Triangle l1 l2 l3) + area (Polygon rest)
  where
    ((x1,y1):(x2,y2):(x3,y3):rest) = vertices
    l1 = sqrt((x1-x2)^2+(y1-y2)^2)
    l2 = sqrt((x1-x3)^2+(y1-y3)^2)
    l3 = sqrt((x2-x3)^2+(y2-y3)^2)

data Date = Day Int Int Int

age :: Date -> Date -> Int
age (Day d m y) (Day cd cm cy)
  = ((cd + cm*30 + cy*365)-(d + m*30 + y*365)) `div` 365

type Database = [UStaff]
data Sex = Male
         | Female

type Empdata = (String, Sex, String, Int)

data SupportType = Admin
          | Tech
data Subject = Systems
             | Software
             | Theory
type Courses = [Int]
data UStaff = Teaching Empdata Subject Courses
              | Support Empdata SupportType

name :: UStaff -> String
name (Teaching (n,_,_,_) _ _) = n
name (Support (n,_,_,_) _ ) = n

