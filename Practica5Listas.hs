-- LTP, Practica 5 (2026-27)
-- Ejercicios 1-11 y ampliacion 17-19.
-- Cargar con: ghci Practica5Listas.hs
module Practica5Listas where

-- 1. Decimal a binario (bit menos significativo primero).
-- Precondicion del enunciado: n >= 0.
decBin :: Int -> [Int]
decBin x = if x < 2 then [x]
           else (x `mod` 2) : decBin (x `div` 2)

-- 2. Binario a decimal (bit menos significativo primero).
-- Se supone una lista no vacia con ceros y unos, como en el enunciado.
binDec :: [Int] -> Int
binDec (x:[]) = x
binDec (x:xs) = x + binDec xs * 2
binDec [] = error "binDec: la lista de bits debe ser no vacia"

-- 3. Divisores positivos de un entero positivo.
-- Por convenio, devolvemos [] para n <= 0 (0 tiene infinitos divisores).
divisors :: Int -> [Int]
divisors n = [d | d <- [1..n], n `mod` d == 0]

-- 4. Pertenencia de un entero a una lista.
member :: Int -> [Int] -> Bool
member _ [] = False
member n (x:xs) = n == x || member n xs

-- 5. Comprobacion de primalidad y lista de los n primeros primos.
isPrime :: Int -> Bool
isPrime n = n > 1 && divisors n == [1,n]

primes :: Int -> [Int]
primes n = take n [x | x <- [2..], isPrime x]

-- 6. Seleccionar los valores pares.
selectEven :: [Int] -> [Int]
selectEven [] = []
selectEven (x:xs)
    | even x    = x : selectEven xs
    | otherwise = selectEven xs

-- 7. Seleccionar los elementos en indices 0, 2, 4, etc.
selectEvenPos :: [Int] -> [Int]
selectEvenPos [] = []
selectEvenPos [x] = [x]
selectEvenPos (x:_:xs) = x : selectEvenPos xs

-- 8. Insertar en lista ordenada y ordenacion por insercion.
ins :: Int -> [Int] -> [Int]
ins n [] = [n]
ins n (x:xs)
    | n <= x    = n : x : xs
    | otherwise = x : ins n xs

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

-- 9. Duplicar todos los elementos usando map.
doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

-- 10. map y filter definidos con listas intensionales.
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

-- 11. Base de datos de prestamos de biblioteca.
type Person = String
type Book = String
type Database = [(Person,Book)]

exampleBase :: Database
exampleBase =
    [ ("Alicia", "El nombre de la rosa")
    , ("Juan", "La hija del canibal")
    , ("Pepe", "Odesa")
    , ("Alicia", "La ciudad de las bestias")
    ]

obtain :: Database -> Person -> [Book]
obtain dBase thisPerson =
    [book | (person,book) <- dBase, person == thisPerson]

-- Anadir el prestamo al principio de la base de datos.
borrow :: Database -> Book -> Person -> Database
borrow dBase book person = (person,book) : dBase

-- Eliminar la primera aparicion del prestamo indicado.
return' :: Database -> (Person,Book) -> Database
return' [] _ = []
return' (p:ps) target
    | p == target = ps
    | otherwise   = p : return' ps target

-- 17. Numero de apariciones de un entero.
repeated :: Int -> [Int] -> Int
repeated x xs = length (filter (==x) xs)

-- 18. Concatenacion de una lista de listas.
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

-- 19. Suma de los cuadrados de los numeros pares de una lista.
square :: Int -> Int
square x = x * x

misteriosa :: [Int] -> Int
misteriosa = sum . map square . filter even
