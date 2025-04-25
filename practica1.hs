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