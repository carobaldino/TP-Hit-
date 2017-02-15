mayorSegun :: Ord b => (a -> b) -> a -> a -> Bool
mayorSegun funcion v1 v2 = funcion v1 > funcion v2
--1
menorSegun :: Ord b => (a -> b) -> a -> a -> Bool
--menorSegun funcion v1 v2 = not(mayorSegun funcion v1 v2)
menorSegun funcion v1 = (not.mayorSegun funcion v1) v2

type Pista = [Tramo]
type Tramo = (Int,CorrenccionVelocidad)

type CorrecionVelocidad = (Chocobo -> Int)
type Chocobo = (Int,Int,Int)

--2
--a
tiempo :: Chocobo -> Tramo -> Int
tiempo chocobo tramo = fst tramo / snd tramo chocobo

--b
tiempoTotal :: Pista -> Chocobo -> Int
tiempoTotal pista chocobo = (sum.map (tiempo chocobo)) pista

--3
type Jinete = (String,Chocobo)

podio :: Pista -> [Jinete] -> [Jinete]
podio pista jinetes = take 3.quickSort (menorSegunTiempo pista)
--podio pista jinetes = take 3 (quickSort (menorSegunTiempo pista) jinetes)

menorSegunTiempo :: Pista -> Jinete -> Jinete -> Bool
menorSegunTiempo pista = menorSegun (tiempoTotal pista.snd)
--menorSegunTiempo pista jinete1 jinete2 = menorSegun (tiempoTotal pista.snd) jinete1 jinete2
--menorSegunTiempo pista jinete1 jinete2 = menorSegun (tiempoTotal pista) (snd jinete1) (snd jinete2)

--4
--a
elMejorDelTramo :: Tramo -> [Jinete] -> String
elMejorDelTramo tramo = nombre.head.podio [tramo]
--elMejorDelTramo tramo jinetes = (nombre.head.podio [tramo]) jinetes
--b
elMasWinner :: Pista -> [Jinete] -> String
elMasWinner pista jinetes = (elQueMasAparece.listaDeNombresGanadores pista) jinetes

elMejorDelTramo :: Tramo -> [Jinete] -> String
listaDeNombresGanadores pista jinetes = map (elMejorDelTramo pista)