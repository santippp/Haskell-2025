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
data RBT a = E | T Color (RBT a) a (RBT a) deriving(Show)

fromOrdList' :: Ord a => [a] -> Color -> RBT a 
fromOrdList' [] _ = E 
fromOrdList' xs c = let m    = div (length xs) 2 
                        x    = xs !! m -- x es el elemento ed la mitad de la lista
                        ant  = take m xs
                        post = drop (m+1) xs
                        c'   = if c == R then B else R -- garantiza que se alterne entre negro y rojo
                        in T c (fromOrdList' ant c') x (fromOrdList' post c')

fromOrdList :: Ord a => [a] -> RBT a
fromOrdList xs = fromOrdList' xs B

makeBlack :: RBT a -> RBT a
makeBlack (T _ l x r) = T B l x r

insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
        where ins x E = T R E x E
              ins x (T c l y r) | x < y = balance c (ins x l) y r
                                | x > y = balance c l y (ins x r)
                                | otherwise = T c l y r 

balance :: Color -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance B l y r = T B l y r

lbalance :: RBT a -> a -> RBT a -> RBT a
labalance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lbalance l y r = T B l y r

rbalance :: RBT a -> a -> RBT a -> RBT a
rabalance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance l y r = T B l y r

insert' :: Ord a => a -> RBT a -> RBT a
insert' x t = makeBlack (ins x t)
        where ins x E = T R E x E
              ins x (T c l y r) | x < y = lbalance (ins x l) y r
                                | x > y = rbalance l y (ins x r)
                                | otherwise = T c l y r 
type Rank = Int
data Heap a = Em | N Rank a (Heap a) (Heap a) deriving(Show)

rank :: Heap a -> Int
rank Em = 0
rank (N r _ _ _) = r

makeH :: a -> Heap a -> Heap a -> Heap a
makeH x a b = if rank a >= rank b then N (rank b + 1) x a b -- elijo el rango del arbol mas corto, porque ese es el rango del arbol que construyo
                             else N (rank a + 1) x a b 

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 Em = h1
merge Em h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) = if x <= y then makeH x a1 (merge b1 h2) 
                                                    else makeH y a2 (merge b2 h1)

convertir :: a -> Heap a
convertir x = N 1 x Em Em

{- fromlist ::Ord a => [a] -> Heap a
fromlist [] = Em
fromlist (x:xs) = merge (convertir x) (fromlist xs) -} -- tiene complejidad O(n log n)

fromlist :: Ord a => [a] -> Heap a
fromlist xs = fromList' (map convertir xs)
  where
    fromList' [] = Em
    fromList' [h] = h
    fromList' (h1:h2:hs) = fromList' (merge h1 h2 : hs)
    

