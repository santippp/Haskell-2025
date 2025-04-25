type RGB = (Int, Int , Int)

mezclar :: RGB -> RGB -> RGB 
mezclar (r1, g1, b1) (r2, g2, b2) = (div (r1+r2) 2, div (g1+g2) 2, div (b1+b2) 2)

type Linea = ([Char], Int)

vacia :: Linea
vacia = ([], 0)

moverIzq :: Linea -> Linea
moverIzq (xs, n) | n > 0 = (xs, n-1)
                 | otherwise = (xs, n)

moverDer :: Linea -> Linea
moverDer (xs, n) | n == length(xs) = (xs, n)
                          | otherwise = (xs, n+1)

moverIni :: Linea -> Linea
moverIni (xs, _) = (xs, 0)

moverFin :: Linea -> Linea
moverFin (xs, _) = (xs, (length xs))

insertar :: Char -> Linea -> Linea
insertar c (xs, p) = ((take p xs ++ [c] ++ drop p xs), p+1)

borrar :: Linea -> Linea
borrar (xs, p) | p > 0 = ((take p xs ++ drop (p+1) xs), p-1)
               | otherwise = (xs, p)

data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving (Show, Eq)

member :: Ord a => a -> Bin a -> Bool
member _ Hoja = False
member a (Nodo l x r) | a == x = True
                      | a < x = member a l
                      | otherwise = member a r

minimum' :: Bin a -> a
minimum' (Nodo Hoja x r) = x
minimum' (Nodo l x r) = minimum' l

maximum' :: Bin a -> a
maximum' (Nodo l x Hoja) = x
maximum' (Nodo l x r) = maximum' r

checkBST :: Ord a => Bin a -> Bool
checkBST Hoja = True
checkBST (Nodo Hoja x Hoja) = True
checkBST (Nodo Hoja x l@(Nodo ll lx lr)) = x > lx && maximum' l < x && checkBST l
checkBST (Nodo r@(Nodo rl rx rr) x Hoja) = x > rx && minimum' r < x && checkBST r
checkBST (Nodo l@(Nodo _ lx _) x r@(Nodo _ rx _)) = x > lx && x < rx && maximum' l < x && minimum' r > x && checkBST l && checkBST r