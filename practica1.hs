import Data.List
import Data.List (sort, group)

borrarUlt :: [a] -> [a]
borrarUlt [] = []
borrarUlt [_] = []
borrarUlt (x:xs) = x : borrarUlt xs

collect :: (Eq k) => [(k, v)] -> [(k, [v])]
collect xs = foldr insertar [] xs
  where
    insertar (k, v) [] = [(k, [v])]
    insertar (k, v) ((k', vs):rest)
      | k == k'   = (k, v:vs) : rest
      | otherwise = (k', vs) : insertar (k, v) rest

serie :: [a] -> [[a]]
serie xs = map (\i -> take i xs) [0..length xs]

paresIguales :: Int -> Int -> Int -> Int -> Bool
paresIguales a b c d = (a == b && c == d) || (a == c && b == d) || (a == d && b == c)

isosceles :: Int -> Int -> Int -> Bool
isosceles a b c = a == b || b == c || a == c

ror :: [a] -> Int -> [a]
ror xs 0 = xs
ror xs n = go n xs []
            where 
                go 0 ys acc = ys ++ acc
                go _ [] acc = acc
                go i (y:ys) acc = go (i - 1) ys (acc ++ [y])

upto :: Int -> Int -> [Int]
upto n m | n > m = []
         | otherwise = go n m []
          where
            go n m acc | n <= m = go (n+1) m (acc ++ [n])
                       | otherwise = acc

eco :: [Char] -> [Char]
eco xs = go 0 xs []
        where
          go _ [] acc = acc
          go p (x:xs) acc = go (p+1) xs (acc ++ function p x)

function :: Int -> Char -> [Char]
function 0 a = [a]
function n a = a : function (n-1) a