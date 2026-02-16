module Practica01 where

--TIPOS ALGEBRAICOS

--Ejercicio 1
data Shape = Circle Float | --representa el radio
            Square Float | --representa un lado
            Rectangle Float Float| --representa base y altura
            Triangle Float | --representa un lado
            Trapeze Float Float Float --representa base mayor, base menor y altura
            deriving (Show)

instance Eq Shape where
    s1 == s2 = area s1 == area s2


--Funcion que calcula el area de las figuras
area :: Shape -> Float
area (Circle x) = pi * (x**2)
area (Square x) = x ** 2
area (Rectangle x y) = x * y
area (Triangle x) = (sqrt(3)/4) * (x**2)
area (Trapeze x y z) = ((x+y)*z)/2



--Funcion que calcula el perimetro de las figuras
perimeter :: Shape -> Float
perimeter (Circle x) = pi * x * 2
perimeter (Square x) = 4 * x
perimeter (Rectangle x y) = 2*x + 2*y
perimeter (Triangle x) = 3*x
perimeter (Trapeze x y z) = x + y + 2*sqrt(z**2 + ((y-x)/2)**2)

--Ejercicio 2 (Les toca arreglar el sinonimo)
type Point = (Float, Float)

-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (x_1, y_1) (x_2, y_2) = sqrt((x_2 - x_1)**2 + (y_2 - y_1)**2) 

--Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0  x = distance x (0, 0)

--Ejercicio 3
data Haskellium = Haskellium {name :: String,
                              lastName1 :: String,
                              lastName2 :: String,
                              location :: Point,
                              houseShape :: Shape} deriving Show

--Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son = undefined

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost = undefined

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork = undefined

--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: String -> Bool
palindromo = undefined

--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr = undefined

--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia = undefined

--ARBOLES

--Implementacion

data OneTwoTree a = Void | Node a (OneTwoTree a) | Branch a (OneTwoTree a) (OneTwoTree a)

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma = undefined