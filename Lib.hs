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


 
data NombrePalos = Putter | Madera | Hierros deriving (Eq,Show)

{-data Palitos = UnPalo {
  tipo :: NombrePalos, 
  habilidad ::  Habilidad, 
} deriving (Eq,Show)-}

data RangoHierro = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 deriving(Eq,Show)

palo :: NombrePalos -> Habilidad -> RangoHierro -> Tiro
palo Putter hab _ = UnTiro{velocidad = 10, precision = precisionJugador hab * 2, altura = 0 }
palo Madera hab _ = UnTiro{velocidad = 100, precision = precisionJugador hab / 2, altura = 5 }
palo Hierro hab n = UnTiro {velocidad = fuerzaJugador hab * n , precision = precisionJugador hab * n, altura = max (n-3) 0 }

palos :: [NombrePalos]
palos = [Putter, Madera, Hierro]


golpe :: UnJugador -> NombrePalos -> RangoHierro -> Tiro
golpe jugador paloAUsar = palo paloAUsar (habilidad jugador)

