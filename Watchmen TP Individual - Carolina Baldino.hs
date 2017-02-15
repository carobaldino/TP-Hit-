algunosVigilantes = [ ("El Comediante", ["Fuerza"], 1942), 
                      ("Buho Nocturno", ["Lucha", "Ingenierismo"], 1963), 
                      ("Rorschach", ["Perseverancia", "Deduccion", "Sigilo"], 1964), 
                      ("Espectro de Seda", ["Lucha", "Sigilo", "Fuerza"], 1962), 
                      ("Ozimandias", ["Inteligencia", "Mas Inteligencia Aun"], 1968), 
                      ("Buho Nocturno", ["Lucha", "Inteligencia", "Fuerza"], 1939), 
                      ("Espectro de Seda", ["Lucha", "Sigilo"], 1940) ]
                      
agentesDelGobierno = [("Jack Bauer","24"), 
                      ("El Comediante", "Watchmen"), 
                      ("Dr. Manhattan", "Watchmen"), 
                      ("Liam Neeson", "Taken")]                                   
{--
1) HACIENDO HISTORIA..
"En 1949 se firmó el acta de Keene, en la que se obligaba a los viejos vigilantes a retirarse...    
 ◘acta de Keene: se van del grupo los héroes viejos (para los que existe al menos un sucesor) mientras que el sucesor permanece.                  
--}                                       
type Vigilante = (String,[String],Integer)

nombreHeroe (n,_,_) = n
anioHeroe (_,_,e) = e

actaKeene :: [Vigilante] -> [Vigilante]
actaKeene vigilantes = filtrarLosQueNoSeRepiten vigilantes ++ filtrarSucesores vigilantes  


filtrarLosQueNoSeRepiten :: [Vigilante] -> [Vigilante]
filtrarLosQueNoSeRepiten vigilantes = filter (losQueNoSeRepiten vigilantes) vigilantes 

losQueNoSeRepiten :: [Vigilante] -> Vigilante -> Bool
losQueNoSeRepiten vigilantes vigilante = esVerdaderoMenosDeDosVeces (map (tienenElMismoNombre vigilante)  vigilantes)

tienenElMismoNombre :: Vigilante -> Vigilante -> Bool
tienenElMismoNombre vigilante heroe = nombreHeroe vigilante == nombreHeroe heroe

esVerdaderoMenosDeDosVeces :: [Bool] -> Bool
esVerdaderoMenosDeDosVeces listaDeBool = length (filter (==True) listaDeBool) < 2 



filtrarSucesores :: [Vigilante] -> [Vigilante]
filtrarSucesores vigilantes = filter (sucesores vigilantes) vigilantes

sucesores :: [Vigilante] -> Vigilante -> Bool
sucesores vigilantes vigilante = any (esSucesor vigilante) vigilantes

esSucesor :: Vigilante -> Vigilante -> Bool
esSucesor vigilante heroe = (nombreHeroe vigilante == nombreHeroe heroe) && (anioHeroe vigilante > anioHeroe heroe)   


{--
...Tiempo después, en 1959, hubo un accidente en un área de investigación de campo intrínseco...    
◘accidente de laboratorio: en un año dado, aparece un nuevo vigilante, el Doctor Manhattan. Tiene una única habilidad que es la 
manipulación de la materia a nivel atómico.               
--}

accidenteDeLaboratorio :: [Vigilante] -> [Vigilante]
accidenteDeLaboratorio vigilantes = actaKeene (vigilantes ++ [("Dr. Manhattan",["Manipulacion de la materia a nivel atomico"],1959)])


{--
...Éstos dos eventos mejoraron notablemente la participación de UZA en la Guerra de Vietnam, que sucedió a continuación...  
◘guerra de Vietnam: a los vigilantes que además son agentes del gobierno les agrega Cinismo como habilidad, a los restantes no.           
--}

type Agente = (String,String)

nombreAgente (n,_) = n

guerraDeVietnam :: [Vigilante] -> [Vigilante]
guerraDeVietnam vigilantes = agregarCinismo (filtrarALosQueSonAgentes (accidenteDeLaboratorio vigilantes) agentesDelGobierno) ++ (filtrarALosQueNoSonAgentes (accidenteDeLaboratorio vigilantes) agentesDelGobierno) 


filtrarALosQueSonAgentes :: [Vigilante] -> [Agente] -> [Vigilante]
filtrarALosQueSonAgentes vigilantes agentes = filter (esAgente agentes) vigilantes

esAgente :: [Agente] -> Vigilante -> Bool
esAgente agentes vigilante = any (tieneMismoNombre vigilante) agentes

tieneMismoNombre :: Vigilante -> Agente -> Bool
tieneMismoNombre vigilante agente  = nombreAgente agente == nombreHeroe vigilante



filtrarALosQueNoSonAgentes :: [Vigilante] -> [Agente] -> [Vigilante]
filtrarALosQueNoSonAgentes vigilantes agentes = filter (noEsAgente agentes) vigilantes

noEsAgente :: [Agente] -> Vigilante -> Bool
noEsAgente agentes vigilante = all (noTieneMismoNombre vigilante) agentes

noTieneMismoNombre :: Vigilante -> Agente -> Bool
noTieneMismoNombre vigilante agente = nombreAgente agente /= nombreHeroe vigilante



agregarCinismo :: [Vigilante] -> [Vigilante]
agregarCinismo listaDeVigilantesQueSonAgentes = map (sumarHabilidad) listaDeVigilantesQueSonAgentes

sumarHabilidad :: Vigilante -> Vigilante
sumarHabilidad (nombre,[habilidades],anio) = (nombre,[habilidades] ++ ["Cinismo"],anio)


{--
...En NiuShork el comediante muere y como consecuencia de este nefasto accidente se destruye la ciudad."    
◘muerte de un vigilante (es un "retiro" forzoso, digamos... Deja de pertenecer a la agrupación) 
(mueren/se retiran todos los que se llamen de igual manera)
◘destrucción de NiuShork: muere Rorschach y se retira el Dr Manhattan         
--}

destruccionDeNiuShork :: [Vigilante] -> [Vigilante]
destruccionDeNiuShork = retirarElDrManhattan.matarARorschach.matarAlComediante.guerraDeVietnam 


matarAlComediante :: [Vigilante] -> [Vigilante]
matarAlComediante vigilantes = filter noEsElComediante vigilantes

noEsElComediante:: Vigilante -> Bool
noEsElComediante vigilante = nombreHeroe vigilante /= "El Comediante"


matarARorschach :: [Vigilante] -> [Vigilante]
matarARorschach vigilantes = filter noEsRorschach vigilantes

noEsRorschach :: Vigilante -> Bool
noEsRorschach vigilante = nombreHeroe vigilante /= "Rorschach"


retirarElDrManhattan :: [Vigilante] -> [Vigilante]
retirarElDrManhattan vigilantes = filter noEsDrManhattan vigilantes

noEsDrManhattan :: Vigilante -> Bool
noEsDrManhattan vigilante = nombreHeroe vigilante /= "Dr. Manhattan"


{--
2) GRANDES HEROES:
Hacer las funciones y mostrar ejemplos de invocación para poder calcular:              
--}  

--nombreDelSalvador: Dado un conjunto de vigilantes, obtener el nombre de aquél que más habilidades tenga luego de la destrucción de NiuShork.
habilidad :: Vigilante -> [String]
habilidad (_,h,_) = h

nombreDelSalvador :: [Vigilante] -> String
nombreDelSalvador = nombreHeroe.head.elMasHabil.destruccionDeNiuShork

elMasHabil :: [Vigilante] -> [Vigilante] 
elMasHabil vigilantes = filter (elQueTieneMasHabilidades vigilantes) vigilantes 

elQueTieneMasHabilidades :: [Vigilante] -> Vigilante -> Bool
elQueTieneMasHabilidades vigilantes vigilante = any (mayor vigilante) vigilantes

mayor :: Vigilante -> Vigilante -> Bool
mayor vigilante heroe = length (habilidad vigilante) > length (habilidad heroe)


--elElegido: Recibiendo a varios vigilantes, devolver la primera habilidad del vigilante cuyo nombre está formado por más palabras, 
--a continuación de la guerra de Vietnam.
elElegido :: [Vigilante] -> String
elElegido = nombreHeroe.head.nombreFormadoPorMasPalabras.guerraDeVietnam

nombreFormadoPorMasPalabras :: [Vigilante] -> [Vigilante]
nombreFormadoPorMasPalabras vigilantes = filter (esDeMayorPalabras vigilantes) vigilantes

esDeMayorPalabras :: [Vigilante] -> Vigilante -> Bool
esDeMayorPalabras vigilantes vigilante = all (mayorLargo vigilante) vigilantes

mayorLargo :: Vigilante -> Vigilante -> Bool
mayorLargo vigilante heroe = length (nombreHeroe vigilante) >= length (nombreHeroe heroe)


--patriarca: A partir de una lista de vigilantes, saber la edad del más antiguo vigilante, habiendo firmado previamente el acta de Keene.
patriarca :: [Vigilante] -> Integer
patriarca = anioHeroe.head.elMasAntiguo.actaKeene

elMasAntiguo :: [Vigilante] -> [Vigilante] 
elMasAntiguo vigilantes = filter (elMasViejo vigilantes) vigilantes

elMasViejo :: [Vigilante] -> Vigilante -> Bool
elMasViejo vigilantes vigilante = all (esElMasGrande vigilante) vigilantes

esElMasGrande :: Vigilante -> Vigilante -> Bool
esElMasGrande vigilante heroe = anioHeroe vigilante <= anioHeroe heroe
