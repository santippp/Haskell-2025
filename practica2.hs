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


data Clist a = EmptyCL | CUnit a | Consnoc a (Clist a) a deriving (Show, Eq)

headCL :: Clist a -> a
headCL (CUnit x) = x
headCL (Consnoc x _ _) = x

tailCL :: Clist a -> Clist a
tailCL (CUnit _) = EmptyCL
tailCL (Consnoc x EmptyCL y) = CUnit y
tailCL (Consnoc x xs y) = Consnoc (headCL xs) (tailCL xs) y

isEmptyCL :: Clist a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit :: Clist a -> Bool
isCUnit (CUnit _) = True
isCUnit _ = False

reverseCL :: Clist a -> Clist a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x xs y) = Consnoc y (reverseCL xs) x

cons :: Clist a -> a -> Clist a
cons EmptyCL e = CUnit e
cons (CUnit x) e = Consnoc e EmptyCL x
cons (Consnoc x xs y) e = Consnoc e (cons xs x) y

snoc :: Clist a -> a -> Clist a
snoc (EmptyCL) e = CUnit e
snoc (CUnit x) e = Consnoc x EmptyCL e
snoc (Consnoc x xs y) e = Consnoc x (snoc xs y) e

borrarUCL :: Clist a -> Clist a 
borrarUCL (CUnit x) = EmptyCL
borrarUCL (Consnoc x xs y) = cons xs x

borrarPCL :: Clist a -> Clist a
borrarPCL (CUnit x) = EmptyCL
borrarPCL (Consnoc x xs y) = snoc xs y

initsCL :: Clist a -> Clist (Clist a)
initsCL EmptyCL = CUnit EmptyCL 
initsCL xs = snoc (initsCL (borrarUCL xs)) xs

lastCL :: Clist a -> Clist (Clist a)
lastCL EmptyCL = CUnit EmptyCL 
lastCL xs = cons (lastCL (borrarPCL xs)) xs



data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp deriving(Show)

eval :: Aexp -> Int
eval (Num n) = n
eval (Prod a1 a2) = eval a1 * eval a2
eval (Div a1 a2) = eval a1 `div` eval a2 

seval :: Aexp -> Maybe Int
seval (Num x) = Just x
seval (Prod e1 e2) = case (seval e1) of
                    Just x -> case (seval e2) of
                                Just y -> Just (x * y)
                                Nothing -> Nothing
                    Nothing -> Nothing 
seval (Div e1 e2) = case seval e2 of
                      Just x -> case(seval e2) of
                                  Just y -> if y == 0 then Nothing 
                                                      else Just (x `div` y)
                                  Nothing -> Nothing 
                      Nothing -> Nothing

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


completo :: a -> Int -> Bin a
completo x 0 = Hoja
completo x n = let subarbol = completo x (n-1)
                in Nodo subarbol x subarbol

balanceado :: a -> Int -> Bin a
balanceado _ n | n <= 0 = Hoja
balanceado x n = 
  let tamañoIzq = (n - 1) `div` 2
      tamañoDer = (n - 1) - tamañoIzq
  in Nodo (balanceado x tamañoIzq) x (balanceado x tamañoDer)

member' :: Ord a => a -> a -> Bin a -> Bool
member' c x Hoja = c == x
member' c x (Nodo l y r) | x > y = member' c x r
                      | otherwise = member' y x l

memberprime :: Ord a => a -> Bin a -> Bool
memberprime x t@(Nodo l c r) = member' c x t

data Color = R | B deriving(Show, Eq)
data RBT a = E | N Color (RBT a) a (RBT a) deriving(Show)

fromOrdList' :: Ord a => [a] -> Color -> RBT a 
fromOrdList' [] _ = E 
fromOrdList' xs c = let m    = div (length xs) 2
                        x    = xs !! m
                        ant  = take m xs
                        post = drop (m+1) xs
                        c'   = if c == R then B else R
                        in N c (fromOrdList' ant c') x (fromOrdList' post c')

fromOrdList :: Ord a => [a] -> RBT a
fromOrdList xs = fromOrdList' xs B