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

between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

----------------------------------------------
---- Resolución del ejercicio
----------------------------------------------


 
data NombrePalos = Putter | Madera | Hierro Number deriving (Eq,Show)

{-data Palitos = UnPalo {
  tipo :: NombrePalos, 
  habilidad ::  Habilidad, 
} deriving (Eq,Show)-}



palo :: NombrePalos -> Habilidad -> Tiro
palo Putter hab = UnTiro{velocidad = 10, precision = precisionJugador hab * 2, altura = 0 }
palo Madera hab = UnTiro{velocidad = 100, precision = precisionJugador hab / 2, altura = 5 }
palo (Hierro n) hab = UnTiro {velocidad = fuerzaJugador hab * n , precision = precisionJugador hab * n, altura = max (n-3) 0 }

palos :: [NombrePalos]
palos = [Putter, Madera] ++ 


golpe :: UnJugador -> NombrePalos -> RangoHierro -> Tiro
golpe jugador paloAUsar = palo paloAUsar (habilidad jugador)

data TipoObstaculo = Tunel | Laguna | Hoyo deriving (Eq,Show)

obstaculo :: TipoObstaculo -> Tiro -> Tiro
obstaculo Tunel tiro 
  | precision tiro > 90 = UnTiro {velocidad = velocidad tiro * 2, precision = 100, altura = 0}
  | otherwise = UnTiro {velocidad = 0, precision = 0, altura = 0}
obstaculo Laguna tiro
  | velocidad tiro > 80 && altura tiro < 5 && altura tiro > 1 = 
  | otherwise = t 