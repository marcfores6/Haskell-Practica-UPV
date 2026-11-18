-- LTP, Practica 5: ejercicios 12, 13 y 20.
-- Este tipo Tree se diferencia del Tree del ejercicio 22.
module ArbolesTree where

data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving (Show, Eq)

-- 12. Arbol simetrico (intercambiando izquierda y derecha).
symmetric :: Tree a -> Tree a
symmetric (Leaf x) = Leaf x
symmetric (Branch left right) =
    Branch (symmetric right) (symmetric left)

-- 13. Lista no vacia a arbol.
-- Elegimos una construccion recursiva hacia la derecha.
listToTree :: [a] -> Tree a
listToTree [] = error "listToTree: la lista debe ser no vacia"
listToTree [x] = Leaf x
listToTree (x:xs) = Branch (Leaf x) (listToTree xs)

-- 13. Arbol a lista, de izquierda a derecha.
treeToList :: Tree a -> [a]
treeToList (Leaf x) = [x]
treeToList (Branch left right) =
    treeToList left ++ treeToList right

-- 20. Contar las hojas de un arbol.
numleaves :: Tree a -> Int
numleaves (Leaf _) = 1
numleaves (Branch left right) =
    numleaves left + numleaves right
