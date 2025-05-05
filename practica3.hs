data Nat = Cero | Succ Nat deriving(Show)

int2Nat :: Int -> Nat
int2Nat 0 = Cero
int2Nat x = Succ (int2Nat (x-1))

sumNat :: Nat -> Nat -> Nat
sumNat x Cero = x
sumNat n1 (Succ n2) = Succ (sumNat n1 n2)

nat2int :: Nat -> Int
nat2int Cero = 0
nat2int (Succ x) = 1 + nat2int x

data Arb = E | H Int | Node Arb Arb deriving(Show , Eq)

data Cmd = L | R deriving(Show , Eq)

select :: [Cmd] -> Arb -> Arb
select [] x = x
select (x : xs) (Node a b) | x == L = select xs a
                        | x == R = select xs b

enum :: Arb -> [[Cmd]]
enum (H _) = [[]]
enum (Node a b) = [L : path | path <- enum a] ++ [R : path | path <- enum b]

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

findMin :: Heap a -> a
findMin (N _ x a b) = x

rank :: Heap a -> Int
rank Em = 0
rank (N r _ _ _) = r

checkHeap :: Ord a => Heap a -> Bool
checkHeap Em = True
checkHeap (N _ x Em Em) = True
checkHeap (N _ x l@(N _ lx ll lr) Em) = x <= lx && checkHeap l
checkHeap (N _  x Em r@(N _ rx rl rr)) = x <= rx && checkHeap r
checkHeap (N _ x l@(N _ lx ll lr) r@(N _ rx rl rr)) = x <= lx && x <= rx && checkHeap l && checkHeap r

checklheap :: Ord a => Heap a -> Bool
checklheap Em = True
checklheap h@(N _ x left right) = checkHeap h && rank left >= rank right && checklheap left && checklheap right

ata Color = R | B deriving(Show, Eq)
data RBT a = E | T Color (RBT a) a (RBT a) deriving(Show)

prop1 :: RBT a -> Bool
prop1 E = True
prop1 (N c _ _ _) = c == B

prop2 :: RBT a -> Bool
prop2 E = True
prop2 (T R (T R _ _ _) _ _) = False
prop2 (T R _ _ (T R _ _ _)) = False
prop2 (T _ l _ r) = prop2 l && prop2 r

prop3 :: RBT a -> Bool
prop3 t = case countBlacks t [] of
    []      -> True  -- Árbol vacío, trivialmente válido
    (x:xs) -> allEqual x xs
  where
    -- Función auxiliar para verificar igualdad
    allEqual :: Int -> [Int] -> Bool
    allEqual _ [] = True
    allEqual n (y:ys) = n == y && allEqual n ys

    -- Contador de nodos negros en caminos
    countBlacks :: RBT a -> [Color] -> [Int]
    countBlacks E xs = [length (filter (==B) xs)]
    countBlacks (T c l _ r) xs = countBlacks l (c:xs) ++ countBlacks r (c:xs)


checkRBT :: Ord a => RBT a -> Bool
checkRBT t = prop1 t && prop2 t && prop3 t