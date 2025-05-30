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
type Palo = Habilidad -> Tiro
palo :: String -> Number -> Palo
palo "putter" _ hab = UnTiro {velocidad = 10, precision = precisionJugador hab*2, altura = 0}
palo "madera" _ hab = UnTiro {velocidad = 100, precision = precisionJugador hab/2, altura = 5}
palo "hierro" n hab = UnTiro {velocidad = fuerzaJugador hab*n, precision = precisionJugador hab/n, altura = (n-3) `max` 0}


putter :: Palo
putter = palo "putter" 0
madera :: Palo
madera = palo "madera" 0
hierro :: [Palo]
hierro = map (palo "hierro") [1..10]

palos ::[Palo]
palos = [putter,madera] ++ hierro

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = UnTiro {velocidad, precisionJugador }

{- Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.
Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0. -}
