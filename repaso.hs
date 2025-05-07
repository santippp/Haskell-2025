{- type Linea = ([Char], Int)

vacia :: Linea 
vacia = ([], 0)

moverIzq :: Linea -> Linea
moverIzq (xs,y) | y /= 0 = (xs, (y-1))
                | otherwise = (xs, y)

moverDer :: Linea -> Linea
moverDer (xs,y) | y < (length xs) = (xs, (y+1))
                | otherwise = (xs, y)  

moverIni :: Linea -> Linea
moverIni (xs, y) = (xs, 0)       

moverFin :: Linea -> Linea
moverFin (xs, y) = (xs, (length xs))       

insertar :: Char -> Linea -> Linea
insertar c (xs, y) = let ys = (take y xs ++ [c] ++ drop y xs) 
                     in (ys , (y+1))

borrar :: Linea -> Linea
borrar (xs, y) | y == 0 = (xs, y)
               | otherwise = let ys = take (y-1) xs ++ drop y xs
                             in (ys, (y-1))

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving(Show)

headCL :: CList a -> a
headCL (CUnit a) = a
headCL (Consnoc x _ _) = x

tailCL :: CList a -> CList a
tailCL (CUnit _) = EmptyCL
tailCL (Consnoc x xs y) = Consnoc (headCL xs) (tailCL xs) y

isEmpty :: CList a -> Bool
isEmpty EmptyCL = True
isEmpty _ = False

isUnit :: CList a -> Bool
isUnit (CUnit _) = True
isUnit _ = False

reverseCL :: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x xs y) = Consnoc y (reverseCL xs) x

snoc :: CList a -> a -> CList a
snoc EmptyCL y = CUnit y
snoc (CUnit x) y = Consnoc x EmptyCL y
snoc (Consnoc a xs b) y = Consnoc a (snoc xs b) y

cons :: CList a -> a -> CList a
cons EmptyCL x = CUnit x
cons (CUnit a) x = Consnoc x EmptyCL a
cons (Consnoc a xs b) x = Consnoc x (cons xs a) b

borrarUCL :: CList a -> CList a
borrarUCL (CUnit x) = EmptyCL
borrarUCL (Consnoc x xs y) = cons xs x

borrarPCL :: CList a -> CList a
borrarPCL (CUnit x) = EmptyCL
borrarPCL (Consnoc x xs y) = snoc xs y

initsCL :: CList a -> CList (CList a)
initsCL EmptyCL = CUnit EmptyCL
initsCL xs = snoc (initsCL (borrarUCL xs)) xs

lastCL :: CList a -> CList (CList a)
lastCL EmptyCL = CUnit EmptyCL
lastCL ys = cons (lastCL (borrarPCL ys)) ys

unir :: CList a -> CList a -> CList a
unir EmptyCL y = y
unir x EmptyCL = x
unir (CUnit x) (CUnit y) = Consnoc x EmptyCL y
unir (CUnit x) (Consnoc a ys b) = Consnoc x (cons ys a) b
unir (Consnoc a xs b) (CUnit y) = Consnoc a (snoc xs b) y
unir (Consnoc a1 xs b1) (Consnoc a2 ys b2) = Consnoc a1 (unir (snoc xs b1) (cons ys a2)) b2

concatCL :: CList (CList a) -> CList a
concatCL EmptyCL = EmptyCL
concatCL (CUnit x) = x
concatCL (Consnoc x xs y) = unir x (concatCL (snoc xs y)) 

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp deriving(Show)

eval :: Aexp -> Int
eval (Num a) = a
eval (Prod a b) = (eval a) * (eval b)
eval (Div a b) = div (eval a) (eval b)

evalM :: Aexp -> Maybe Int
evalM (Num x) = Just x
evalM (Prod x y) = case (evalM x) of 
          Just x -> case (evalM y) of
                    Just y -> Just (x*y)
                    Nothing -> Nothing
          Nothing -> Nothing
evalM (Div x y) = case (evalM x) of 
          Just x -> case (evalM y) of
                    Just y -> if y == 0 then Nothing else Just (div x y)
                    Nothing -> Nothing
          Nothing -> Nothing

data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving (Show, Eq)

maximum' :: Bin a -> a
maximum' (Nodo Hoja x Hoja) = x
maximum' (Nodo _ _ r) = maximum' r

minimum' :: Bin a -> a
minimum' (Nodo Hoja x Hoja) = x
minimum' (Nodo l x r) = minimum' l

checkBST :: Ord a => Bin a -> Bool
checkBST Hoja = True
checkBST (Nodo Hoja x Hoja) = True
checkBST (Nodo l@(Nodo _ lx _) x Hoja) = maximum' l < x && lx < x && checkBST l 
checkBST (Nodo Hoja x r@(Nodo _ rx _)) = minimum' r > x && rx > x && checkBST r 
checkBST (Nodo l@(Nodo _ lx _) x r@(Nodo _ rx _)) = maximum' l < x && lx < x && checkBST l && minimum' r > x && rx > x && checkBST r


completo :: a -> Int -> Bin a
completo _ 0 = Hoja
completo x n = let tree = completo x (n-1)
               in Nodo tree x tree

balanceado :: a -> Int -> Bin a
balanceado _ 0 = Hoja
balanceado x n = let tizq = div n 2
                     tder = n - tizq
                 in Nodo (balanceado x tizq) x (balanceado x tder)

member :: Ord a => a -> Bin a -> Bool
member x Hoja = False
member x (Nodo l y r) | x == y = True
                      | x < y = member x l 
                      | otherwise = member x r 

memberprime :: Ord a => a -> Bin a -> Bool
memberprime x t@(Nodo l c r) = member' x c t 
                            where
                                member' x cand Hoja = x == cand
                                member' x cand (Nodo l c r) | x < c = member' x cand l 
                                                            | otherwise = member' x c r

data Color = R | B deriving(Show, Eq)
data RBT a = E | T Color (RBT a) a (RBT a) deriving(Show)


fromOrdList :: Ord a => [a] -> RBT a
fromOrdList xs = go xs B 
            where
                go [] _ = E
                go xs c = let m = div (length xs) 2
                              medio = xs !! m
                              izq = take m xs
                              der = drop (m+1) xs
                              c' = if c == R then B else R 
                              in T c (go izq c') medio (go der c')

makeBlack :: RBT a -> RBT a
makeBlack (T _ l x r) = T B l x r

balance :: Ord a => Color -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

insertTeoria :: Ord a => a -> RBT a -> RBT a
insertTeoria x t = makeBlack (ins x t)
                where
                    ins x E = T R E x E 
                    ins x (T c l y r) | x < y = balance c (ins x l) y r
                                      | x > y = balance c l y (ins x r)
                                      | otherwise = T c l y r

lbalance :: Ord a => Color -> RBT a -> a -> RBT a -> RBT a
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
labalance c l a r = T c l a r

rbalance :: Ord a => Color -> RBT a -> a -> RBT a -> RBT a
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rabalance c l a r = T c l a r

insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack(ins x t)
            where
                ins x E = T R E x E 
                ins x (T c l y r) | x < y = lbalance c (ins x l) y r 
                                  | x > y = rbalance c l y (ins x r) 
                                  | otherwise = T c l y r

type Rank = Int
data Heap a = Em | N Rank a (Heap a) (Heap a) deriving(Show)

rank :: Heap a -> Int
rank Em = 0
rank (N r _ _ _) = r

makeH :: a -> Heap a -> Heap a -> Heap a
makeH x a b = if rank a >= rank b then N (rank b + 1) x a b -- elijo el rango del arbol derecho porq es el camino mas corto
                                  else N (rank a + 1) x b a

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 Em = h1
merge Em h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) = if x <= y then makeH x a1 (merge b1 h2) 
                                                    else makeH y a2 (merge b2 h1)

toheap :: a -> Heap a
toheap x = N 1 x Em Em

fromlistheap :: Ord a => [a] -> Heap a
fromlistheap [] = Em
fromlistheap xs = aux (map toheap xs)
        where
            aux [] = Em
            aux [x] = x
            aux (x:y:ys) = aux ((merge x y) : ys)
-}

--practica complementaria:

data Nat = Cero | Succ Nat deriving(Show)

int2Nat :: Int -> Nat
int2Nat 0 = Cero
int2Nat n = Succ (int2Nat (n-1))

sumNat :: Nat -> Nat -> Nat
sumNat Cero y = y
sumNat x Cero = x
sumNat (Succ x) (Succ y) = Succ (Succ (sumNat x y))

nat2int :: Nat -> Int
nat2int Cero = 0
nat2int (Succ x) = 1 + nat2int x

data Arb = E | H Int | Node Arb Arb deriving(Show)

data Cmd = L | R deriving(Show, Eq)

select :: [Cmd] -> Arb -> Arb
select [] x = x
select _ E = E
select _ (H n) = (H n)
select (x:xs) (Node l r) | x == L = select xs l
                      | x == R = select xs r 

enum :: Arb -> [[Cmd]]
enum E = []
enum (H _) = [[]]
enum (Node l r) = map (L :) (enum l) ++ map (R :) (enum r)

data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving (Show, Eq)

calcularNodo :: Bin a -> Int -> Int
calcularNodo Hoja _ = 0
calcularNodo (Nodo l _ r) n | n == 0 = 1
                            | n > 0 = calcularNodo l (n-1) + calcularNodo r (n-1)

altura :: Bin a -> Int
altura Hoja = 0
altura (Nodo l _ r) = 1 + max (altura l) (altura r)

checkbalanced :: Bin a -> Bool
checkbalanced (Nodo Hoja _ Hoja) = True
checkbalanced (Nodo a _ b) = abs (altura a - altura b) <= 1 && checkbalanced a && checkbalanced b

type Rank = Int
data Heap a = Em | N Rank a (Heap a) (Heap a) deriving(Show, Eq)

rank :: Heap a -> a
rank (N r _ _ _ ) = r

checkHeap :: Heap a -> Bool
checkHeap Em = True
checkHeap (N _ x Em Em) = True
checkHeap (N _ x l@(N lx ll lr) Em) = x <= lx && checkHeap l 
checkHeap (N _ x Em r@(N rx rl rr)) = x <= rx && checkHeap r
checkHeap (N _ x l@(N lx ll lr) r@(N rx rl rr)) = x <= rx && checkHeap r && x <= lx && checkHeap l 

checklheap :: Heap a -> Bool
checklheap h@(N r x l r) = rank l >= rank r && checkHeap h 
