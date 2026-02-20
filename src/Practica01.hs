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
son padre madre nombre = Haskellium nombre (lastName1 padre) (lastName1 madre) (location padre) (houseShape padre)

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost haskellium = (2.5 * perimeter (houseShape haskellium)) + area (houseShape haskellium)

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork  haskellium = if from0 (location haskellium) < 300 then from0 (location haskellium) / 30 else from0 (location haskellium) / 70 

--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: String -> Bool
palindromo [] = True
palindromo [x] = True
palindromo (x:xs) = if x /= ultimo(xs) then False else palindromo (menosUltimo xs)

{-
Funcion auxiliar ultimo
Regresa el último elemento de una lista
Para usar en la función de palíndromo
-}
ultimo :: [a] -> a
ultimo [a] = a
ultimo (x:xs) = ultimo xs

{-
funcion auxiliar que regresa la lista sin el último elemento
Para usar en las llamadas recursivas de la funcion palindromo
-}
menosUltimo :: [a] -> [a]
menosUltimo [] = []
menosUltimo [x] = []
menosUltimo (x:xs) = (x:menosUltimo xs)

--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z [] = z
myFoldr f z l = myFoldr f ( f (ultimo l) z) (menosUltimo l)

--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia  [] = [[]]
conjuntoPotencia (x:xs) = [x:ys | ys <- conjuntoPotencia xs] ++ (conjuntoPotencia xs)

--ARBOLES

--Implementacion

data OneTwoTree a = Void | Node a (OneTwoTree a) | Branch a (OneTwoTree a) (OneTwoTree a)

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma Void = 0
suma (Node a t) = a + suma t
suma (Branch a t_1 t_2) = a + suma t_1 + suma t_2