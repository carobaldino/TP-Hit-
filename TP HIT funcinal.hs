data Archivo = Archivo {
    nombre :: String,
    contenido :: String
    }deriving(Show,Eq)
    
--1
tamanioArchivo :: Archivo -> Int
tamanioArchivo = (*8).length.contenido

--2
estaVacio :: Archivo -> Bool
estaVacio = null.contenido

--3 
cantidadDeLineas :: Archivo -> Int
cantidadDeLineas = length.lines.contenido

--4
hayLineaBlanca :: Archivo -> Bool
hayLineaBlanca = (any esLineaBlanca).lines.contenido
    
esLineaBlanca :: String -> Bool
esLineaBlanca = all isSpace

--5
esExtencionHs :: Archivo -> Bool
esExtencionHs = (== ".hs").reverse.(take 3).reverse.nombre

--6
renombrar :: String -> Archivo -> Archivo
renombrar nuevoNombre (Archivo nombre contenido) = Archivo {nombre = nuevoNombre, contenido = contenido}

--Modificacion de archivos
type Modificacion = Archivo -> Archivo
--7
agregarLinea :: Int -> String -> Modificacion
agregarLinea numLinea linea = modificarContenido (agregarElemento numLinea linea)  

modificarContenido :: ([String] -> [String]) -> Modificacion
modificarContenido modificacion (Archivo nombre contenido) = Archivo nombre ((unlines.modificacion.lines) contenido) 

agregarElemento :: Int -> String -> [String] -> [String]
agregarElemento posicion elemento lista = take (posicion - 1) lista ++ [elemento] ++ drop (posicion -1) lista

--8
quitarLinea :: Int -> Modificacion
quitarLinea numLinea = modificarContenido (quitarElemento numLinea) 

quitarElemento :: Int -> [String] -> [String]
quitarElemento posicion lista = take (posicion - 1) lista ++ drop posicion lista

--9
reemplazarLinea :: Int -> String -> Modificacion
reemplazarLinea numLinea nuevaLinea archivo = agregarLinea numLinea nuevaLinea (quitarLinea numLinea archivo)

--10
buscarYReemplazar :: String -> String -> Modificacion
buscarYReemplazar palabraBuscada nuevaPalabra archivo = modificarContenidov2 (buscar nuevaPalabra palabraBuscada) archivo

modificarContenidov2 :: ([String] -> [String]) -> Modificacion
modificarContenidov2 funcion (Archivo nombre contenido) = Archivo nombre (unwords.funcion.words $ contenido)  

buscar :: String -> String -> [String] -> [String]
buscar nuevaPalabra palabraBuscada contenido = map (reemplazarSiCorresponde nuevaPalabra palabraBuscada) contenido

reemplazarSiCorresponde :: String -> String -> String -> String
reemplazarSiCorresponde nuevaPalabra palabraBuscada palabra 
	| palabra == palabraBuscada = nuevaPalabra
	| otherwise = palabra


