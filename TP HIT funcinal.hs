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

