-- LTP, Practica 5: ejercicio 22.
-- Tipo Tree DISTINTO al de los ejercicios 12, 13 y 20.
module ArbolesPropiedades where

data Tree a = Branch a (Tree a) (Tree a) | Void
    deriving (Show, Eq)

-- 22. Contar los nodos que satisfacen un predicado.
countProperty :: (a -> Bool) -> Tree a -> Int
countProperty _ Void = 0
countProperty p (Branch x left right) =
    (if p x then 1 else 0)
    + countProperty p left
    + countProperty p right
