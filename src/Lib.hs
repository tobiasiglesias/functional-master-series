module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String -- Solamente interesa el nombre

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

filtrarVocales :: Palabra -> Palabra
filtrarVocales palabra = filter esVocal palabra

ultimasNLetras :: Int -> Palabra -> Palabra
ultimasNLetras n palabra = sacoNLetras (length palabra - n) palabra

sacoNLetras :: Int -> Palabra -> Palabra
sacoNLetras = sacoNElementos 

sacoNElementos :: Int -> [a] -> [a]
sacoNElementos n (x:xs)
    | n == 0    = (x:xs)
    | otherwise = sacoNElementos (n-1) xs

rimaAsonante :: Palabra -> Palabra -> Bool
rimaAsonante palabra1 palabra2 = ultimas2Vocales palabra1 == ultimas2Vocales palabra2

ultimas2Vocales :: Palabra -> Palabra
ultimas2Vocales = ultimasNLetras 2 . filtrarVocales

rimaConsonante :: Palabra -> Palabra -> Bool
rimaConsonante palabra1 palabra2 = ultimasNLetras 3 palabra1 == ultimasNLetras 3 palabra2

dosPalabrasRiman :: Palabra -> Palabra -> Bool
dosPalabrasRiman palabra1 palabra2
    | palabra1 == palabra2 = False
    | otherwise            = rimaAsonante palabra1 palabra2 || rimaConsonante palabra1 palabra2 

-- -- 1-a:
-- - dos palabras iguales == falso
-- - dos palabras que riman riman asonante == verdadero
-- - dos palabras que riman consonantemente == verdadero 
-- - dos palabras que no riman de ninguna manera == falso

-- CONJUGACIONES:

conjugacionPorMedioDeRimas :: Verso -> Verso -> Bool
conjugacionPorMedioDeRimas verso1 verso2 = dosPalabrasRiman (ultimaPalabra verso1) (ultimaPalabra verso2)

ultimaPalabra :: Verso -> Palabra
ultimaPalabra = last . words

conjugacionPorMedioDeAnadiplosis :: Verso -> Verso -> Bool
conjugacionPorMedioDeAnadiplosis verso1 verso2 = ultimaPalabra verso1 == primeraPalabra verso2

primeraPalabra :: Verso -> Palabra
primeraPalabra = head . words 

dosVersosRiman :: Verso -> Verso -> Bool
dosVersosRiman verso1 verso2 = conjugacionPorMedioDeRimas verso1 verso2 || conjugacionPorMedioDeAnadiplosis verso1 verso2

--PATRONES

patronSimple :: Int -> Int -> Estrofa -> Bool
patronSimple posicion1 posicion2 estrofa = dosVersosRiman (versoNumero posicion1 estrofa) (versoNumero posicion2 estrofa)

versoNumero :: Int -> Estrofa -> Verso
versoNumero n estrofa = head . sacoNElementos (n - 1) $ estrofa

estrofaTest = ["esta rima es fácil como patear un penal","solamente tiene como objetivo servir de ejemplo","los versos del medio son medio fríos","porque el remate se retoma al final"]


