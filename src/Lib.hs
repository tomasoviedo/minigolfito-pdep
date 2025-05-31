module Lib where
import PdePreludat
-- Modelo inicial

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles

between n m x = x `elem` [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

----------------------------------------------
---- Resolución del ejercicio
----------------------------------------------

razosuave = UnTiro 15 97 0
razofuerte = UnTiro 100 57 0
altosuave = UnTiro 12 97 5
altofuerte = UnTiro 120 57 2
tiroDetenido = tiroDetenido
data NombrePalos = Putter | Madera | Hierro Number deriving (Eq,Show)

palo :: NombrePalos -> Habilidad -> Tiro
palo Putter hab = UnTiro{velocidad = 10, precision = precisionJugador hab * 2, altura = 0 }
palo Madera hab = UnTiro{velocidad = 100, precision = precisionJugador hab / 2, altura = 5 }
palo (Hierro n) hab = UnTiro {velocidad = fuerzaJugador hab * n , precision = precisionJugador hab * n, altura = max (n-3) 0 }

palos :: [NombrePalos]

palos = [Putter, Madera] ++ map Hierro[1..10]



golpe :: Jugador -> NombrePalos -> Tiro
golpe jugador paloAUsar = palo paloAUsar (habilidad jugador)



{-obstaculo :: TipoObstaculo -> Tiro -> Tiro
obstaculo Tunel tiro
  | precision tiro > 90 = UnTiro {velocidad = velocidad tiro * 2, precision = 100, altura = 0}
  | otherwise = UnTiro {velocidad = 0, precision = 0, altura = 0}
  -}

-- 3

data TipoObstaculo = Tunel | Laguna Number | Hoyo deriving (Eq,Show)

obstaculo :: TipoObstaculo -> Tiro ->  Tiro
obstaculo  Tunel (UnTiro velocidad precision altura)
  | precision > 90 = UnTiro (velocidad*2) 100 0
  | otherwise = tiroDetenido
obstaculo  (Laguna n) (UnTiro velocidad precision altura)
  | velocidad > 80 && between 1 5 altura = UnTiro velocidad precision (altura/n)
  | otherwise = tiroDetenido
obstaculo  Hoyo (UnTiro velocidad precision altura)
  | between 5 20 velocidad && precision >=95 = tiroDetenido
  | otherwise = UnTiro velocidad precision altura

pruebaPalo :: Jugador -> TipoObstaculo -> NombrePalos -> Bool
pruebaPalo jugador Hoyo tipoPalos  = obstaculo Hoyo (golpe jugador tipoPalos) == tiroDetenido
pruebaPalo jugador Tunel tipoPalos = obstaculo Tunel (golpe jugador tipoPalos) /= tiroDetenido
pruebaPalo jugador (Laguna n) tipoPalos = obstaculo (Laguna n) (golpe jugador tipoPalos) /= tiroDetenido

-- obstaculo Hoyo (golpe jugador tipoPalos)
-- obstaculo Hoyo . golpe jugador TipoPalos (?)
-- (golpe jugador . obstaculo Hoyo ) TipoPalos

-- 4
-- a

palosUtiles :: Jugador -> TipoObstaculo -> [NombrePalos]
palosUtiles jugador tipoObstaculo = filter (pruebaPalo jugador tipoObstaculo) palos

obstaculoPasado :: Tiro -> TipoObstaculo -> Bool
obstaculoPasado tiro Hoyo = obstaculo Hoyo tiro == tiroDetenido 
obstaculoPasado tiro Tunel = obstaculo Tunel tiro /= tiroDetenido 
obstaculoPasado tiro (Laguna n) = obstaculo (Laguna n) tiro /= tiroDetenido 

-- b

consecutivos :: Tiro -> [TipoObstaculo] -> Number
-- consecutivos tiro obstaculos = length (takeWhile (foldl obstaculoPasado tiro) obstaculos)
consecutivos tiro [] = 0
consecutivos tiro (obstaculo1 : obstaculos) 
  | obstaculoPasado tiro obstaculo1
      = 1 + consecutivos (obstaculo obstaculo1 tiro) obstaculos
  | otherwise = 0

--foldl obstaculoPasado tiro obstaculos

{-Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.
Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, y una lista con dos túneles con rampita seguidos de un hoyo, el resultado sería 2 ya que la velocidad al salir del segundo túnel es de 40, por ende no supera el hoyo.
BONUS: resolver este problema sin recursividad, teniendo en cuenta que existe una función takeWhile :: (a -> Bool) -> [a] -> [a] que podría ser de utilidad.-}

