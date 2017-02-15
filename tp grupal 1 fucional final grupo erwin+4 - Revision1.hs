import Data.Char

data Archivo =  Archivo {nombre :: String,
                         contenido :: String
                        } deriving (Show,Eq)
   
   
type Modificacion = Archivo -> Archivo                
    
--Archivo de Prueba 
unArchivo = Archivo "listalarga.hs" "la casa de juan es linda\n la casa de mi abuela no\n"           

--Operaciones Sobre Archivos---------------------------------------------------------------------------------------------------------------------------
--1----------------------------------------------------------------------------------------------------------------------------------------------------
tamanioEnBytes :: Archivo -> Int
--tamanioEnBytes (Archivo nombre contenido) = (*8) (length contenido)
{-|  
    -- Fecha: [ 25/04 ] --
    -- Estado: ✓
 
      Usamos composición con la funcion 'contenido' que nos brina Archivo y obviamos el parametro 'archivo' 
-} 
tamanioEnBytes = (*8).length.contenido

-- *Main> tamanioEnBytes unArchivo
-- 400


--2----------------------------------------------------------------------------------------------------------------------------------------------------
estaVacio :: Archivo -> Bool
--estaVacio (Archivo nombre contenido) = null contenido
{-|  
    -- Fecha: [ 25/04 ] --
    -- Estado: ✓
 
       Usamos composición con la funcion 'contenido' que nos brina Archivo y obviamos el parametro 'archivo'   
-} 
estaVacio = null.contenido

-- *Main> estaVacio unArchivo
-- False

--3----------------------------------------------------------------------------------------------------------------------------------------------------
cantidadDeLineas :: Archivo -> Int 
--cantidadDeLineas (Archivo nombre contenido) = length (lines contenido)
{-|  
    -- Fecha: [ 25/04 ] --
    -- Estado: ✓
 
      Usamos composición y omitimos el parametro con point free   
-} 
cantidadDeLineas = length.lines.contenido

-- *Main> cantidadDeLineas unArchivo
-- 2

--4----------------------------------------------------------------------------------------------------------------------------------------------------
lineasBlancas :: Archivo -> Bool
--lineasBlancas file = any (== True) (listaDeCaracteresBlancos file)

--listaDeCaracteresBlancos :: Archivo -> [Bool]
--listaDeCaracteresBlancos (Archivo nombre contenido) = map isSpace contenido 

{-|  
    -- Fecha: [ 25/04 ] --
    -- Estado: ✓
 
     
     Hicimos de vuelta la funcion, componiendo y usando point free 

-} 
lineasBlancas = (foldl (||) False).map verificarLineaALinea.lines.contenido 

verificarLineaALinea :: String -> Bool 
verificarLineaALinea = (foldl (&&) True).map isSpace        

-- *Main> lineasBlancas unArchivo
--False        
    
--5----------------------------------------------------------------------------------------------------------------------------------------------------
esExtencionHs :: Archivo -> Bool
--esExtencionHs (Archivo nombre contenido) = drop ( length (nombre) - 3) nombre == ".hs"      

{-|  
    -- Fecha: [ 25/04 ] --
    -- Estado: ✓
 
     Usamos composición y la funcion reverse  
-} 
esExtencionHs = (== ".hs").reverse.take 3.reverse.nombre 

-- *Main> esExtencionHs unArchivo
--True

--Modificaciones Sobre Archivos------------------------------------------------------------------------------------------------------------------------
--6----------------------------------------------------------------------------------------------------------------------------------------------------
renombrarArchivo :: String -> Modificacion                                                              
renombrarArchivo nuevoNombre (Archivo nombre contenido) = Archivo {nombre = nuevoNombre, contenido = contenido}   

-- *Main> renombrarArchivo "listacorta.hs" unArchivo
-- Archivo {nombre = "listacorta.hs", contenido = "la casa de juan es linda\n la casa de mi abuela no\n"}                                                              

--7----------------------------------------------------------------------------------------------------------------------------------------------------
agregarLinea :: Int -> String -> Modificacion
--agregarLinea numLinea lineaNueva (Archivo nombre contenido) = Archivo {nombre = nombre, contenido = unlines (aux1 unArchivo numLinea lineaNueva)}
--aux1 (Archivo nombre contenido)  numLinea lineaNueva = take (numLinea - 1) (lines contenido) ++ lines lineaNueva ++ drop (numLinea - 1) (lines contenido)
{-|  
    -- Fecha: [ 25/04 ] --
    -- Estado: ✓
 
     Delegamos y compusimos 

-} 
agregarLinea numLinea lineaNueva = modificarContenido (nuevoContenido (numLinea - 1) lineaNueva)

modificarContenido :: (String -> String) -> Modificacion
modificarContenido modificacion (Archivo nombre contenido)= Archivo {nombre=nombre, contenido=modificacion contenido}

nuevoContenido :: Int -> String -> String -> String
nuevoContenido numLinea lineaNueva contenido = lineasHasta numLinea contenido ++ lineaNueva ++"\n"++ lineasDesde numLinea contenido

lineasHasta :: Int -> String -> String
lineasHasta numLinea = lineunline (take numLinea)

lineasDesde :: Int -> String -> String
lineasDesde numLinea = lineunline (drop numLinea)

lineunline :: ([String] -> [String]) -> String -> String
lineunline funcion = unlines.funcion.lines

-- *Main> agregarLinea 1 "lalalalalalalalalal" unArchivo
-- Archivo {nombre = "listalarga.hs", contenido = "lalalalalalalalalal\nla casa de juan es linda\n la casa de mi abuela no\n"}

--8----------------------------------------------------------------------------------------------------------------------------------------------------
quitarLinea :: Int -> Modificacion
--quitarLinea numLinea (Archivo nombre contenido) = Archivo {nombre = nombre, contenido = unlines (aux2 unArchivo numLinea) }       
--aux2 (Archivo nombre contenido) numLinea = take (numLinea - 1) (lines contenido) ++ drop numLinea (lines contenido)
{-|  
    -- Fecha: [ 25/04 ] --
    -- Estado: ✓
 
      Mismma corrección que arriba
-} 
quitarLinea = modificarContenido.quitar 

quitar :: Int -> String -> String
quitar numLinea contenido = lineasHasta (numLinea-1) contenido ++ lineasDesde numLinea contenido

-- *Main> quitarLinea 2 unArchivo
-- Archivo {nombre = "listalarga.hs", contenido = "la casa de juan es linda\n"}

--9----------------------------------------------------------------------------------------------------------------------------------------------------
reemplazarLinea :: Int -> String -> Modificacion
--reemplazarLinea numLinea contenidoNuevo (Archivo nombre contenido) = Archivo {nombre = nombre, contenido = unlines (aux3 unArchivo numLinea contenidoNuevo) }    
--aux3 (Archivo nombre contenido) numLinea contenidoNuevo = take (numLinea - 1) (lines contenido) ++ lines contenidoNuevo ++ drop numLinea (lines contenido)
{-|  
    -- Fecha: [ 25/04 ] --
    -- Estado: ✓
 
      Mismma corrección que arriba.
-} 
reemplazarLinea numLinea lineaNueva = modificarContenido (cambiar numLinea lineaNueva)

cambiar :: Int -> String -> String -> String
cambiar numLinea lineaNueva contenido = lineasHasta (numLinea-1) contenido ++ lineaNueva ++"\n "++ lineasDesde numLinea contenido

-- *Main> reemplazarLinea 1 "holaholahola" unArchivo
-- Archivo {nombre = "listalarga.hs", contenido = "holaholahola\n la casa de mi abuela no\n"}

--10---------------------------------------------------------------------------------------------------------------------------------------------------
buscarYreemplazar :: String -> String -> Modificacion
--buscarYreemplazar palabraAbuscar nuevaPalabra (Archivo nombre contenido) = Archivo {nombre = nombre, contenido = unwords (map (reemplazar palabraAbuscar nuevaPalabra) (words contenido)) }   
--reemplazar palabraAbuscar nuevaPalabra elementoDeLaLista | (elementoDeLaLista == palabraAbuscar) = nuevaPalabra
--                                                         | otherwise = elementoDeLaLista
{-|  
    -- Fecha: [ 25/04 ] --
    -- Estado: X
 
     Idem 7,8,9 y COMPONEEEEEEEER!! http://i0.kym-cdn.com/entries/icons/original/000/000/063/Picture_2_c.jpg
-} 
buscarYreemplazar palabraAbuscar nuevaPalabra (Archivo nombre contenido) = Archivo {nombre = nombre,contenido = unwords (map (reemplazar palabraAbuscar nuevaPalabra) (words contenido)) }

reemplazar palabraAbuscar nuevaPalabra elementoDeLaLista | (elementoDeLaLista == palabraAbuscar) = nuevaPalabra
                                                         | otherwise = elementoDeLaLista
-- *Main> buscarYreemplazar "casa" "mansion" unArchivo
-- Archivo {nombre = "listalarga.hs", contenido = "la mansion de juan es linda la mansion de mi abuela no"}
                                                                                                             
--11---------------------------------------------------------------------------------------------------------------------------------------------------
wrappear :: Modificacion
wrappear (Archivo nombre contenido) = Archivo {nombre = nombre, 
                                               contenido = unlines (recortar (lines contenido))}

recortar unaLista = concatMap (recortarLineas) unaLista
recortarLineas linea | (length linea) <= 80 = [linea] 
                     | otherwise = [(take 80 linea)] ++ recortar ([drop 80 linea])


{-|  
    -- Fecha: [ 25/04 ] --
    -- Estado: X
 
     No es necesario el paréntesis en recortarLineas, así como tampoco lo es en la guarda.
     El concepto está bien, pero veo que en muchos lados están usando el [] y es porque te lo obliga el concatMap. 
     No se podría pensar de otra forma para no escribirlos al pedo?
-} 


-- *Main> wrappear unArchivo
-- Archivo {nombre = "listalarga.hs", contenido = "la casa de juan es linda\n la casa de mi abuela no\n"}
-- *Main> wrappear Archivo {nombre = "listalarga.hs", contenido = "la casa de juan es linda\n la casa de mi abuela noaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n"}
-- Archivo {nombre = "listalarga.hs", contenido = "la casa de juan es linda\n la casa de mi abuela noaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\naaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\naaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\naaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\naaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n"}

                     
--12---------------------------------------------------------------------------------------------------------------------------------------------------
modificacionEsInutil :: Modificacion -> Archivo -> Bool
modificacionEsInutil funcionQueModifica (Archivo nombre contenido) = (funcionQueModifica (Archivo nombre contenido)) == (Archivo nombre contenido)


{-|  
    -- Fecha: [ 25/04 ] --
    -- Estado: X
 
     Componer. Es necesario escribir el archivo como data? No conviene escribirlo como una unidad? modificacionEsInutil modificacion archivo?
-} 

-- *Main> modificacionEsInutil (renombrarArchivo "listalarga.hs") unArchivo
-- True
-- *Main> modificacionEsInutil (renombrarArchivo "listalarg.hs") unArchivo
-- False
-- *Main> modificacionEsInutil wrappear unArchivo
-- True
-- *Main> modificacionEsInutil (quitarLinea 1) unArchivo
-- False
-- *Main> modificacionEsInutil (reemplazarLinea 1 "se reemplaza") unArchivo
-- False
-- *Main> modificacionEsInutil (buscarYreemplazar "casa" "mansion") unArchivo
-- False

--13---------------------------------------------------------------------------------------------------------------------------------------------------
aplicarRevision :: [Modificacion] -> Archivo -> Archivo
aplicarRevision listaDeFunciones (Archivo nombre contenido)  = foldl (flip ($)) (Archivo nombre contenido) listaDeFunciones

{-|  
    -- Fecha: [ 25/04 ] --
    -- Estado: X
 
    Trabajar archivo sin descomponerlo como data?     
-} 

-- *Main> aplicarRevision [(renombrarArchivo "hoal.hs"),(quitarLinea 1)] unArchivo
-- Archivo {nombre = "hoal.hs", contenido = " la casa de mi abuela no\n"}

--14---------------------------------------------------------------------------------------------------------------------------------------------------

--15---------------------------------------------------------------------------------------------------------------------------------------------------

--16---------------------------------------------------------------------------------------------------------------------------------------------------


