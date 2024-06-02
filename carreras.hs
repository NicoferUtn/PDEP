import Text.Show.Functions
import Data.List(genericLength)

type Carrera = [Auto]
-- Estado en funcional -> Los Autos

data Auto = Auto {
    color :: Color,
    velocidad :: Int,
    distancia :: Int    
} deriving (Show)

type Color = String  --Por facha

type Evento estructura = estructura -> estructura
type PowerUp = Auto -> Evento Carrera
--   PowerUp = Auto -> [Carrera -> Crrera] para mi entnedimiento
auto1 = Auto "Rojo" 10 0
auto2 = Auto "Azul" 10 10
auto3 = Auto "Vede" 10 10

--Punto 1 a
estanCerca :: Auto -> Auto -> Bool
estanCerca autoA autoB = ((< 10) . abs . distanciaEntre autoA) autoB  && sonDistintos autoA autoB

distanciaEntre :: Auto -> Auto -> Int
distanciaEntre autoA autoB = distancia autoA - distancia autoB 

sonDistintos :: Auto -> Auto -> Bool
sonDistintos autoA autoB = color autoA /= color autoB

--Punto 1.b
vaTraquilo :: Auto -> Carrera -> Bool
vaTraquilo = all . estanCerca  

--Punto 1.c
puesto :: Auto -> Carrera -> Int
puesto autoA  = (1+) . autosAdelante autoA 

autosAdelante :: Auto -> Carrera -> Int
autosAdelante auto  = length. map (leVaGanadoA auto)

--Punto 2 a
correrDurante  :: Int -> Evento Auto
correrDurante  tiempo auto = auto {distancia = tiempo * velocidad auto + distancia auto}
-- 2 b.1
--Se refiere a esto ?? sino la veo una funcion inutil si es solo de Int -> Int
alterarVelocidad :: (Int -> Int) -> Evento Auto
alterarVelocidad modificador auto = auto {velocidad = max 0 (modificador (velocidad auto))}

--Punto 2 b.2
bajarleLaVelocidad :: Int -> Evento Auto
bajarleLaVelocidad = alterarVelocidad . subtract 

--Punto 3 
--Funcion dada por la catedera
afectarSegun :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarSegun criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not . criterio) lista

--Punto 3 a
terremoto ::  PowerUp
terremoto  autobase  = afectarSegun (estanCerca autobase) (bajarleLaVelocidad 50)

--Punto 3 b
miguelitos :: Int -> PowerUp
miguelitos velocidadARestar  autobase = afectarSegun (leVaGanadoA autobase) (bajarleLaVelocidad velocidadARestar) 

leVaGanadoA :: Auto -> Auto -> Bool
leVaGanadoA autoBase = (< 0) . distanciaEntre autoBase 

--Punto 3 c
jetPack :: Int -> PowerUp
jetPack tiempo  autobase = afectarSegun (esElMismoAuto autobase) (updatesVelocidadesJetPack tiempo)
                                                                    
esElMismoAuto :: Auto -> Auto -> Bool
esElMismoAuto autobase = not . sonDistintos autobase

updatesVelocidadesJetPack :: Int -> Auto -> Auto
updatesVelocidadesJetPack tiempo = correrDurante (tiempo * 2)

--Punto 4 a
podersAAplicar:: [Evento Carrera]
podersAAplicar = [terremoto auto1 , miguelitos 10 auto2, jetPack 30 auto3]


simularCarrera :: Carrera -> [Evento Carrera] -> [(Int,Color)]
simularCarrera carrera eventos = tablaDePosiciones . aplicarEventos eventos $ carrera


aplicarEventos ::  [Evento Carrera] -> Carrera -> Carrera
aplicarEventos  =  flip $ foldl (flip ($)) 

--foldl $ carrera evento 
--foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f` ...) `f` xn
--(elPuesto auto , color auto)

tablaDePosiciones :: Carrera -> [(Int, Color)]
tablaDePosiciones carrera = map (\x -> (puesto x carrera, color x)) carrera


--Puesto 4 b i
correnTodos :: Int -> Carrera -> Carrera
correnTodos  = map . correrDurante  

--PowerUp = Auto -> [Carrera -> Crrera] para mi entnedimiento
--afectarSegun :: (a -> Bool) -> (a -> a) -> [a] -> [a]

--Punto 4 b ii
usaPowerUp :: PowerUp -> Color -> Carrera -> Carrera
usaPowerUp powerUp color carrea = powerUp (buscarAuto color carrea) carrea

buscarAuto :: Color -> Carrera -> Auto
buscarAuto colorAuto = head . filter (esColor colorAuto)

esColor :: Color -> Auto -> Bool
esColor colorAuto = ( == colorAuto) . color