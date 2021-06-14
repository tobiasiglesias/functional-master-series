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
esVocal = flip elem "aeiouáéíóú"

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
sacoNElementos = drop
-- sacoNElementos :: Int -> [a] -> [a]
-- sacoNElementos n [x] = []
-- sacoNElementos n (x:xs)
--     | n == 0    = (x:xs)
--     | otherwise = sacoNElementos (n-1) xs

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

type Conjugacion = Verso -> Verso -> Bool

conjugacionPorMedioDeRimas :: Conjugacion
conjugacionPorMedioDeRimas verso1 verso2 = dosPalabrasRiman (ultimaPalabra verso1) (ultimaPalabra verso2)

ultimaPalabra :: Verso -> Palabra
ultimaPalabra = last . words

conjugacionPorMedioDeAnadiplosis :: Conjugacion
conjugacionPorMedioDeAnadiplosis verso1 verso2 = ultimaPalabra verso1 == primeraPalabra verso2

primeraPalabra :: Verso -> Palabra
primeraPalabra = head . words 

dosVersosRiman :: Conjugacion
dosVersosRiman verso1 verso2 = conjugacionPorMedioDeRimas verso1 verso2 || conjugacionPorMedioDeAnadiplosis verso1 verso2

--PATRONES

type Patron = Estrofa -> Bool

patronSimple :: Int -> Int -> Patron
patronSimple posicion1 posicion2 estrofa = dosVersosRiman (versoNumero posicion1 estrofa) (versoNumero posicion2 estrofa)

versoNumero :: Int -> Estrofa -> Verso
versoNumero = elementoNumero

elementoNumero :: Int -> [a] -> a
elementoNumero n lista = head . sacoNElementos (n - 1) $ lista

letraNumero :: Int -> Palabra -> Char
letraNumero n palabra 
    | palabra == [] = '#'
    | n >= length palabra = elementoNumero (n - length palabra) palabra
    | otherwise     = elementoNumero n palabra

estrofaTest = ["esta rima es fácil como patear un penal","solamente tiene como objetivo servir de ejemplo","los versos del medio son medio fríos","porque el remate se retoma al final"]

esEsdrujula :: Palabra -> Bool
esEsdrujula palabra = tieneTilde . letraNumero 3 . reverse . filter esVocal $ palabra

patronEsdrujulas :: Patron
patronEsdrujulas = all (esEsdrujula . ultimaPalabra)

patronAnafora :: Patron
patronAnafora estrofa = (== length estrofa) . length . filter (== head (map primeraPalabra estrofa)) . map primeraPalabra $ estrofa

cadena :: Patron
cadena estrofa = (/=versoNoRima) . foldl1 rimar $ estrofa

versoNoRima :: Verso
versoNoRima = "__%%&"

rimar :: Verso -> Verso -> Verso
rimar verso1 verso2
    | dosVersosRiman verso1 verso2 = verso2
    | otherwise                    = versoNoRima

combinaDos :: Patron -> Patron -> Estrofa -> Bool
combinaDos patron1 patron2 estrofa = patron1 estrofa && patron2 estrofa

aabb :: Patron
aabb estrofa = patronSimple 1 2 estrofa && patronSimple 3 4 estrofa

abab :: Patron
abab estrofa = patronSimple 1 3 estrofa && patronSimple 2 4 estrofa

abba :: Patron
abba estrofa = patronSimple 1 4 estrofa && patronSimple 2 3 estrofa

hardcore :: Patron
hardcore estrofa = cadena estrofa && patronEsdrujulas estrofa

-- Una estrofa con infinitos versos no podria ser hardcore ya que deberia evaluar todos los elementos de la lista para ver si son esdrujulas y ademas
-- el fold nunca terminaria

-- Una estrofa con infinitos versos si podria ser abba ya que solo se necesita evaluar 4 versos de la estrofa

data Escena = Escena {
    artista :: Artista,
    estrofa :: Estrofa,
    publicoExaltado :: Bool,
    potencia :: Float
} deriving(Show, Eq)

escenaTest = Escena "Tobias" estrofaTest False 50

type Estilo = Escena -> Escena

aumentarPotenciaEn :: Float -> Estilo
aumentarPotenciaEn n escena = escena {potencia = (1 + (n / 100)) * potencia escena }

gritar :: Estilo
gritar = aumentarPotenciaEn 50

exaltarPublico :: Estilo
exaltarPublico escena = escena {publicoExaltado = True}

tirarTecnicas :: Patron ->  Estilo
tirarTecnicas patron escena
    | puedeTirarPatron patron escena = aumentarPotenciaEn 10 . exaltarPublico $ escena
    | otherwise                      = escena

puedeTirarPatron :: Patron -> Escena -> Bool
puedeTirarPatron patron escena = patron (estrofa escena)

tirarFreestyle :: Artista -> Estrofa -> Estilo -> Escena
tirarFreestyle artista estrofa estilo = estilo (Escena artista estrofa False 1.0)

type Puntos = Float
type Jurado = Escena -> Puntos

alToke' :: Jurado
alToke' (Escena _ estrofa publicoExaltado potencia) = siSumar(aabb estrofa) 0.5 + siSumar (combinaDos patronEsdrujulas abba estrofa) 1.0 + siSumar publicoExaltado 1.0 + siSumar (potencia > 1.5) 2.0

alToke :: Jurado
alToke escena 
    | alToke' escena > 3.0 = 3.0
    | otherwise            = alToke' escena 

siSumar :: Bool -> Float -> Float
siSumar booleano n 
    | booleano  = n 
    | otherwise = 0.0

evaluacion = alToke escenaTest

