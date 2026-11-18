-- Pruebas automaticas de la Practica 5.
-- Ejecutar desde la carpeta del proyecto: runghc Main.hs
module Main where

import qualified Practica5Listas as L
import qualified ArbolesTree as T
import qualified ArbolesBinarios as B
import qualified ArbolesPropiedades as P

check :: (Eq a, Show a) => String -> a -> a -> IO ()
check nombre esperado obtenido =
    if esperado == obtenido
    then putStrLn ("OK  " ++ nombre)
    else error ("FALLO " ++ nombre ++ ": esperado " ++ show esperado ++ ", obtenido " ++ show obtenido)

main :: IO ()
main = do
    check "01 decBin 4" [0,0,1] (L.decBin 4)
    check "02 binDec [0,1,1]" 6 (L.binDec [0,1,1])
    check "03 divisors 24" [1,2,3,4,6,8,12,24] (L.divisors 24)
    check "04 member (si)" True (L.member 1 [1,2,3,4,8,9])
    check "04 member (no)" False (L.member 0 [1,2,3,4,8,9])
    check "05 isPrime 2" True (L.isPrime 2)
    check "05 primes 5" [2,3,5,7,11] (L.primes 5)
    check "06 selectEven" [2,4,8,10] (L.selectEven [1,2,4,5,8,9,10])
    check "07 selectEvenPos" [1,4,8,10] (L.selectEvenPos [1,2,4,5,8,9,10])
    check "08 ins" [0,1,3,4,5,6,7,8,9] (L.ins 5 [0,1,3,4,6,7,8,9])
    check "08 iSort" [0,1,3,4,6,7,8,9] (L.iSort [4,9,1,3,6,8,7,0])
    check "09 doubleAll" [2,4,8,10] (L.doubleAll [1,2,4,5])
    check "10 map'" [2,4,6] (L.map' (*2) [1,2,3])
    check "10 filter'" [2,4] (L.filter' even [1,2,3,4])
    check "11 obtain" ["El nombre de la rosa","La ciudad de las bestias"] (L.obtain L.exampleBase "Alicia")
    check "11 borrow" [("Juan","Dune")] (take 1 (L.borrow L.exampleBase "Dune" "Juan"))
    check "11 return'" 3 (length (L.return' L.exampleBase ("Pepe","Odesa")))
    let t = T.Branch (T.Leaf 3) (T.Branch (T.Leaf 5) (T.Leaf 8))
    check "12 symmetric" (T.Branch (T.Branch (T.Leaf 8) (T.Leaf 5)) (T.Leaf 3)) (T.symmetric t)
    check "13 listToTree/treeToList" [1,2,3] (T.treeToList (T.listToTree [1,2,3]))
    check "14 insTree" (B.Node 5 (B.Node 3 B.Void (B.Node 4 B.Void B.Void)) (B.Node 7 B.Void B.Void)) (B.insTree 4 (B.Node 5 (B.Node 3 B.Void B.Void) (B.Node 7 B.Void B.Void)))
    check "15 creaTree" (B.Node 5 (B.Node 3 B.Void B.Void) (B.Node 7 B.Void B.Void)) (B.creaTree [5,3,7])
    check "16 treeElem (si)" True (B.treeElem 4 B.treeB3)
    check "16 treeElem (no)" False (B.treeElem 9 B.treeB3)
    check "17 repeated" 3 (L.repeated 2 [1,2,3,2,4,2])
    check "18 concat'" [1,2,3,4,8,9] (L.concat' [[1,2],[3,4],[8,9]])
    check "19 misteriosa" 220 (L.misteriosa [1..10])
    check "20 numleaves" 3 (T.numleaves t)
    check "21 dupElem treeB1" B.Void (B.dupElem B.treeB1)
    check "21 dupElem treeB2" (B.Node 10 B.Void B.Void) (B.dupElem B.treeB2)
    check "21 dupElem treeB3" (B.Node 10 (B.Node 6 (B.Node 2 B.Void B.Void) (B.Node 8 B.Void B.Void)) (B.Node 12 B.Void (B.Node 16 B.Void B.Void))) (B.dupElem B.treeB3)
    let propTree = P.Branch 5 (P.Branch 12 P.Void P.Void) P.Void
    check "22 countProperty (>9)" 1 (P.countProperty (>9) propTree)
    check "22 countProperty (>0)" 2 (P.countProperty (>0) propTree)
    putStrLn "Todas las comprobaciones han terminado correctamente."
