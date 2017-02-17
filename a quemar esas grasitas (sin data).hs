type Gimnasta = (Int, Int, Int)
type Ejercicio = Int -> Gimnasta -> Gimnasta
type Rutina = (String, Int, [Ejercicio])

pancho :: Gimnasta
pancho = (40, 120, 1)

andres :: Gimnasta
andres = (22, 80, 6)

relax :: Ejercicio
relax minutos persona = persona

tonificacion (_,_, tonif) = tonif
peso (_, peso, _) = peso
edad (edad, _, _) = edad

saludable gimnasta = not (estaObeso gimnasta) && (tonificacion gimnasta > 5)
estaObeso gimnasta = peso gimnasta > 100 -- abstraccion importante

bajarDePeso (e, peso, t) kilos = (e, peso-kilos, t) -- abstraccion importante
quemarCalorias gimnasta calorias
	| estaObeso gimnasta = bajarDePeso gimnasta (calorias `div` 150)
	| edad gimnasta > 30 && calorias > 200 = bajarDePeso gimnasta 1 --puedo agregar que no esté obeso, pero como no entró en la guarda anterior se puede asumir
	| otherwise = bajarDePeso gimnasta (calorias `div` (edad gimnasta * peso gimnasta))
	
cinta vMin vMax minutos gimnasta = quemarCalorias gimnasta ((vMin+vMax)*minutos `div` 2) -- abstraccion importante, aprovecho y lo defino como algo que puede ser llevado al tipo de ejercicio

caminata :: Ejercicio
caminata = cinta 5 5 -- puedo también pasarle minutos y gimnasta a cinta, pero es lo mismo, ya que estamos...

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta minutos = cinta 6 (6+ minutos `div` 5) minutos -- nuevamente no explicito el parámetro gimnasta aunque podría hacerlo y pasarlo a cinta como último parámetro. No se puede hacer lo mismo con los minutos porque los necesito en otro lugar, no sólo para aplicar como 3er parámetro de cinta

pesas kilos minutos gimnasta
	| minutos > 10 = tonificar (kilos `div` 10) gimnasta
	| otherwise = gimnasta
	
tonificar cuanto (e, p, tonif) = (e, p, tonif+cuanto) -- abstraccion importante

colina inclinacion minutos gimnasta = quemarCalorias gimnasta (minutos*2*inclinacion)

montaña inclinacion minutos = tonificar 1.colina (inclinacion+3) duracionColina.colina inclinacion duracionColina
	where duracionColina = minutos `div` 2
	
-- si bien es posible hacerlo recursivamente de una, es mucho más sencillo delegar la parte recursiva del problema a otra función
realizarRutina (_, duracionTotal, ejercicios) gimnasta = ejercitar (duracionDeCadaEjercicioDeRutina duracionTotal ejercicios) ejercicios gimnasta
duracionDeCadaEjercicioDeRutina duracionTotal = (duracionTotal `div`).length

ejercitar _ [] gimnasta = gimnasta
ejercitar mins (ej:ejs) gimnasta = ejercitar mins ejs (ej mins gimnasta)

-- alternativa más compleja que va directo a la recursividad, hay que ir bajando el tiempo y armar una nueva rutina más corta en cada paso:
realizarRutina' (_,_,[]) gimnasta = gimnasta
realizarRutina' (nombre, duracionTotal, (ej:ejs)) gimnasta = realizarRutina' (nombre, duracionTotal - minutosEjercicio, ejs) (ej minutosEjercicio gimnasta)
	where minutosEjercicio = duracionDeCadaEjercicioDeRutina duracionTotal (ej:ejs)

-- versión con foldl
realizarRutina'' (_, duracionTotal, ejs) gimnasta = foldl (\gim e -> e (duracionDeCadaEjercicioDeRutina duracionTotal ejs) gim)  gimnasta ejs

-- ejemplo de uso de realizarRutina con un ejercicio de cada tipo
-- realizarRutina ("Una rutina", 60, [caminata, entrenamientoEnCinta, pesas 50, colina 4, montaña 6]) pancho

nombre (n, _, _) = n
resumen rutina gimnasta = (nombre rutina, delta peso rutina gimnasta, delta tonificacion rutina gimnasta)
-- notamos que hay una lógica común entre el cálculo de peso perdido y de tonificación ganada, se puede abstraer el cálculo como una función de orden superior
delta f rutina gimnasta = (abs.(f gimnasta -).f.realizarRutina rutina) gimnasta

-- podria ser con lambdas tambien en vez de flip
loHacenSaludable gimnasta = map (flip resumen gimnasta).filter (saludable.flip realizarRutina gimnasta)
loHacenSaludable' gimnasta = map (\rutina -> resumen rutina gimnasta).filter (saludable.(\rutina -> realizarRutina rutina gimnasta))