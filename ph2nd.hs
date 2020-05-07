import Data.Char
--import Prelude hiding ((^))

-- 6.7
-- a.
and' :: [Bool] -> Bool
and' [] = False
and' [True] = True
and' (False:_) = False
and' (True:xs) = and' xs


-- 6.5 
--length :: [a] -> Int
--length [] = 0
--length (x:xs) = 1 + length xs
--
-- length [1, 2, 3]
-- (applying length)
-- 1 + length [2, 3]
-- (applying length)
-- 1 + 1 + length [3]
-- (applying length)
-- 1 + 1 + 1 + length []
-- (applying length)
-- 1 + 1 + 1 + 0
-- (applying +)
-- 3
--
-- drop :: Int -> [a] -> [a]
-- drop 0 xs = xs 
-- drop _ [] = []
-- drop n (_:xs) = drop (n-1) xs
--
-- drop 3 [1, 2, 3, 4, 5]
-- (applying drop)
-- drop 2 [2, 3, 4, 5]
-- (applying drop)
-- drop 1 [3, 4, 5]
-- (applying drop)
-- drop 0 [4, 5]
-- (applying drop)
-- [4, 5]
--
-- init :: [a] -> [a]
-- init [_] = []
-- init (x:xs) = x : init xs
--
-- init [1, 2, 3]
-- (applying init)
-- 1 : init [2, 3]
-- (applying init)
-- 1 : 2 : init [3]
-- (applying init)
-- 1 : 2 : []

-- 6.4
euclid :: Int -> Int -> Int
euclid m n
    | m == n = n
    | m > n = euclid (m - n) n
    | otherwise = euclid (n - m) m

-- 6.3
--(^) :: (Integral b, Num a) => a -> b -> a 
--b ^ 0 = 1
--b ^ e | e > 0 = b * b ^ (e - 1)
--
-- evaluating 2 ^ 3
-- (applying ^)
-- 2 * 2 ^ (3 - 1)
-- (applying ^)
-- 2 * 2 * 2 ^ (2 - 1)
-- (applying ^)
-- 2 * 2 * 2 * 2 ^ (1 - 1)
-- (applying ^)
-- 2 * 2 * 2 * 1
-- (applying *)
-- 8

-- 6.2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n - 1)


-- 6.1
factorial :: Int -> Int
factorial 0 = 1
factorial n | n >= 0 = n * factorial (n - 1)

let2int :: Char -> Int
let2int n
    | isLower n = ord n - ord 'a' 
    | otherwise = ord n - ord 'A'

int2let :: Int -> Char
int2let n
    | n < 26 = chr (ord 'a' + n)
    | otherwise = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
    | isAlpha c = int2let (mod (let2int c + n) 26)
    | otherwise = c

encode :: Int -> String -> String
encode n cs = [ shift n c | c <- cs]

percent :: Int -> Int -> Float
percent n t = fromIntegral n / (fromIntegral t) * 100.0

lowers :: String -> String
lowers xs = [x | x <- xs, isLower x]

toLower' :: String -> String
toLower' xs = [int2let i | i <- map let2int xs]

count :: Char -> String -> Int
count c cs = sum [ 1 | x <- cs, x == c] 

freqs :: String -> [Float]
freqs xs = [percent (count c ls) (length ls) | c <- ['a'..'z']]
            where ls = toLower' xs
{-freqs xs = [percent (count c ls) (length ls) | c <- ['a'..'z']]-}
            {-where ls = lowers(xs)-}

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7,
         7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

chisrq :: [Float] -> [Float] -> Float
chisrq os es = sum [(o - e)^2 / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = [xs !! (mod (i + n) (length xs))| i <- [0..(length xs - 1)]]

--text = "kdvnhoo lv ixq"

--table' = freqs text

--positions :: Eq e => e -> [e] -> [Int]
--positions e xs = [i | i <- [0..(length xs - 1)], xs !! i == e]

find :: Eq a => a -> [(a, b)] -> [b]
find k vs = [b | (a, b) <- vs, a == k]

-- 5.8
positions :: Eq e => e -> [e] -> [Int]
positions e xs = [k | k <- find e (zip xs [0..(length xs - 1)])] 

crack :: String -> [String]
crack xs = [encode (-i) xs | i <- [0..25], elem i (positions (minimum chisqrs) chisqrs)]
    where table' = freqs xs
          chisqrs = [chisrq (rotate i table') table | i <- [0..25]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int, Int)]
square m = [(x, y) | (x, y) <- grid m m, x /= y]

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 == b^2 + c^2]

factors :: Int -> [Int]
factors n = [d | d <- [1..n], mod n d == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum [f | f <- factors x, f /= x]]

-- 5.7
-- concat [[(x, y), (x, y + 1 )] | (x, y) <- [(1, 3), (2, 3)]]

--5.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys] 
