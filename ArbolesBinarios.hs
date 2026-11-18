-- LTP, Practica 5: ejercicios 14, 15, 16 y 21.
module ArbolesBinarios where

data BinTreeInt = Void | Node Int BinTreeInt BinTreeInt
    deriving (Show, Eq)

-- Arboles de ejemplo del PDF.
treeB1 :: BinTreeInt
treeB1 = Void

treeB2 :: BinTreeInt
treeB2 = Node 5 Void Void

treeB3 :: BinTreeInt
treeB3 = Node 5
            (Node 3 (Node 1 Void Void) (Node 4 Void Void))
            (Node 6 Void (Node 8 Void Void))

-- 14. Insertar un valor en un arbol binario de busqueda ordenado.
-- Convencion adoptada: no insertamos duplicados.
insTree :: Int -> BinTreeInt -> BinTreeInt
insTree x Void = Node x Void Void
insTree x (Node n left right)
    | x < n     = Node n (insTree x left) right
    | x > n     = Node n left (insTree x right)
    | otherwise = Node n left right

-- 15. Construir un arbol ordenado insertando de izquierda a derecha.
creaTree :: [Int] -> BinTreeInt
creaTree = foldl (flip insTree) Void

-- 16. Buscar eficazmente usando la ordenacion del arbol.
treeElem :: Int -> BinTreeInt -> Bool
treeElem _ Void = False
treeElem x (Node n left right)
    | x == n    = True
    | x < n     = treeElem x left
    | otherwise = treeElem x right

-- 21. Duplicar cada valor sin cambiar la estructura.
dupElem :: BinTreeInt -> BinTreeInt
dupElem Void = Void
dupElem (Node n left right) =
    Node (2*n) (dupElem left) (dupElem right)
