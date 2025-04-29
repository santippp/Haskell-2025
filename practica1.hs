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

cambios :: Eq a => [a] -> [Int]
cambios xs = [i | i <- [0..length xs - 2], xs !! i /= xs !! (i + 1)]

oblongoNumber :: [Int]
oblongoNumber = [i * (i+1) | i <- [0..100] ]

abundantes :: [Int]
abundantes = [i | i <- [0..100], i < sum(divisores i)]
  where
    divisores x = [i | i <- [1..x-1], x `mod` i == 0]

euler :: Int -> Int
euler n = sum [i | i <- [0..n-1], i `mod` 5 == 0 || i `mod` 3 == 0]

expandir :: [Int] -> [Int]
expandir xs = concat [replicate i i | i <- xs]

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc ->  f x : acc) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs

unzip' :: [(a,b)] -> ([a],[b])
unzip' xs = foldr (\(x,y) acc -> ((x:fst(acc)), (y:snd(acc)))) ([],[]) xs

par2list :: (a,[b]) -> [(a,b)]
par2list (x,ys) = foldr (\y acc -> (x,y):acc) [] ys

maxSec :: [(Int, Int)] -> (Int, Int)
maxSec xs = foldr (\(x,y) max -> if (y-x) > (snd(max)-fst(max)) then (x,y) else max) (head xs) xs

maxL :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxL seg1 seg2 
  | length1 >= length2 = seg1
  | otherwise = seg2
  where
    length1 = abs (snd seg1 - fst seg1) -- Longitud del primer segmento
    length2 = abs (snd seg2 - fst seg2) -- Longitud del segundo segmento

-- Función principal que encuentra el segmento más largo en una lista de segmentos usando fold
maxSec' :: [(Int, Int)] -> (Int, Int)
maxSec' [] = error "La lista no puede estar vacía"
maxSec' segments = foldl1 maxL segments -- Usa foldl1 para reducir la lista utilizando maxL