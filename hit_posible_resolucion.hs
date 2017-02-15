import Data.Char

data Archivo = Archivo { nombre :: String, contenido :: String } deriving (Show, Eq)

unTpGrupal :: Archivo
unTpGrupal = Archivo "tpGrupal.hs" "la casa de mi abuela es linda\n la casa de juan no"

--------------------------------
-- Funciones de calentamiento --
--------------------------------

-- 01
tamanio :: Archivo -> Int
tamanio = (*8).length.contenido

-- 02
estaVacio :: Archivo -> Bool
estaVacio = null.contenido

-- 03
cantidadLineas :: Archivo -> Int
cantidadLineas = length.lines.contenido

-- 04
algunaLineaBlanca :: Archivo -> Bool
algunaLineaBlanca = any (all isSpace).lines.contenido

-- 05
tieneExtensionHS :: Archivo -> Bool
tieneExtensionHS = (==) ".hs".ultimos 3.nombre

ultimos n = reverse.take n.reverse

------------------------
-- Funciones en serio --
------------------------

-- 06
renombrar :: String -> (Archivo -> Archivo)
renombrar nombre (Archivo _ contenido) = Archivo nombre contenido

-- 07
agregarLinea :: String -> Int -> (Archivo -> Archivo)
agregarLinea linea numero = editar (agregarContenido linea (pred numero))

agregarContenido linea numero lineas = antes numero lineas ++ [linea] ++ despues numero lineas

antes numero = fst . splitAt numero
despues numero = snd . splitAt numero

editar funcion (Archivo nombre contenido) = Archivo nombre (unlines.funcion.lines $ contenido)

-- 08
quitarLinea :: Int -> (Archivo -> Archivo)
quitarLinea numero = editar (quitarContenido numero)

quitarContenido numero lineas = (init.antes numero) lineas ++ despues numero lineas

-- 09
reemplazar :: String -> Int -> (Archivo -> Archivo)
reemplazar linea numero = agregarLinea linea numero . quitarLinea numero

-- 10
buscarYReemplazar :: String -> String -> (Archivo -> Archivo)
buscarYReemplazar aBuscar aReemplazar = editar (reemplazarPalabra aBuscar aReemplazar)

reemplazarPalabra aBuscar aReemplazar = map (unwords . reemplazarPalabraSiCorresponde aBuscar aReemplazar . words)

reemplazarPalabraSiCorresponde aBuscar aReemplazar = foldl (nuevoContenidoSegun aBuscar aReemplazar) []

nuevoContenidoSegun aBuscar aReemplazar palabras palabra
    | aBuscar == palabra = palabras ++ [aReemplazar]
    | otherwise          = palabras ++ [palabra]

-- 11
wrappear :: Archivo -> Archivo
wrappear = editar wrappearLineas

wrappearLineas lineas = concatMap (wrapLine 80) lineas

wrapLine _ "" = [""]
wrapLine count linea = take count linea : (wrapLine count . drop count) linea 

-------------------------
-- Funcion de descanso --
-------------------------

-- 12
esInutil :: Modificacion -> Archivo -> Bool
esInutil modificacion archivo = modificacion archivo == archivo

----------------------------------------------------------------
-- Funciones jodidas... pero si te das cuenta no lo son tanto --
----------------------------------------------------------------

-- 13
type Modificacion = Archivo -> Archivo

modificacionesAArchivo :: [Modificacion] -> Archivo -> Archivo
modificacionesAArchivo modificaciones archivo = foldl (flip ($)) archivo modificaciones

type Revision = [(String, [Modificacion])]
type Directorio = [Archivo]

revisionADirectorio :: Revision -> Directorio -> Directorio
revisionADirectorio revision = map (aplicarSiExiste revision)

aplicarSiExiste :: Revision -> Archivo -> Archivo
aplicarSiExiste revision archivo
    | estaEnRevision revision archivo = aplicarRevision revision archivo
    | otherwise                       = archivo

estaEnRevision :: Revision -> Archivo -> Bool
estaEnRevision revision (Archivo nombre _) = any ((==nombre).fst) revision

aplicarRevision :: Revision -> Archivo -> Archivo
aplicarRevision revision archivo = foldl (\ar (_, mods) -> modificacionesAArchivo mods ar) archivo revision

-- 14
archivoMasGrandeDespuesDeRevisionDeDirectorio :: Revision -> Directorio -> Archivo
archivoMasGrandeDespuesDeRevisionDeDirectorio revision = maximumBy tamanio . revisionADirectorio revision

maximumBy f = foldl1 (maxBy f)

maxBy f x y
    | f x > f y = x
    | otherwise = y

-- 15
archivoConMayorDeltaEnSuTamanioDespuesDeUnaRevision :: Revision -> Directorio -> Archivo
archivoConMayorDeltaEnSuTamanioDespuesDeUnaRevision revision directorio = 
    fst . maximumBy deltaTamanios . zip directorio . revisionADirectorio revision $ directorio

deltaTamanios (archivoOriginal, archivoModificado) = abs $ tamanio archivoOriginal - tamanio archivoModificado

-- 16
aplicarRevisionesADirectorios :: [Revision] -> Directorio -> Directorio
aplicarRevisionesADirectorios revisiones directorio = foldl (flip revisionADirectorio) directorio revisiones

