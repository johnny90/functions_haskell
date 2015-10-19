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
   | n==0 = 1
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
