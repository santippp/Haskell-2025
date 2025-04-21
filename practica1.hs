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