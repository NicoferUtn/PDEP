import Text.Show.Functions
import Data.List(genericLength)
import Distribution.Compat.CharParsing (CharParsing(string))
import Language.Haskell.TH (Body, TExp, DocLoc)

--Hora de inicio 18:12

data Plomero = Plomero{
    nombre :: String,
    herramienta :: [Herramienta],
    historial :: [Reparaciones],
    dinero :: Float
} deriving (Show, Eq)

data Herramienta = Herramienta{
    denominacion :: String,
    precio :: Float,
    material :: Material
} deriving (Show, Eq)

data Reparaciones = Reparaciones{ 
    descripcion :: String,
    requerimiento :: Herramienta,
    precioACobrar :: Float
} deriving (Show, Eq)

data Material = Hierro | Madera | Goma | Plastico deriving (Show, Eq)


llaveInglesa = Herramienta "Llave Inglesa" 200 Hierro
destornillador = Herramienta "Destornillador" 0 Plastico
--Punto 1 
mario = Plomero "Mario" [Herramienta "Llave inglesa" 200  Hierro, Herramienta "Martillo" 20 Madera] [] 1200 
wario = Plomero "Wario" llavesInfinitas [] 0.50

filtracionAgua = Reparaciones "Filtracion Agua" llaveInglesa

llavesInfinitas :: [Herramienta]
llavesInfinitas = map (\x -> Herramienta "llaveFrancesa" x Hierro) [1..]

--Punto 2 a.
tieneUnaHerramienta :: String -> Plomero -> Bool
tieneUnaHerramienta nombreHerramienta  = any (\x -> nombreHerramienta == denominacion x ) . herramienta

--Punto 2 b
esMalvadoElPlomero :: Plomero -> Bool
esMalvadoElPlomero plomero = take 2 (nombre plomero) == "Wa" 

--Punto 2 c
puedeComprarHerramienta :: Plomero -> Herramienta -> Bool
puedeComprarHerramienta plomero herramienta = dinero plomero == precio herramienta

--Punto 3
esBuenaLaHerramienta :: Herramienta -> Bool
esBuenaLaHerramienta (Herramienta "Martillo" _ Madera ) = True
esBuenaLaHerramienta (Herramienta "Martillo" _ Goma) = True
esBuenaLaHerramienta (Herramienta _ _ Hierro) = True 
esBuenaLaHerramienta (Herramienta _ precio _) = precio > 10000

--Punto 4 
comprarUnaHerramienta :: Plomero -> Herramienta -> Plomero
comprarUnaHerramienta plomero herramienta 
    | puedeComprarHerramienta plomero herramienta = (agregarHerramienta herramienta . variarDinero (-precio herramienta)) plomero
    | otherwise = plomero 


variarDinero :: Float -> Plomero -> Plomero
variarDinero precioAVariar plomero = plomero {dinero = dinero plomero + precioAVariar}

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta herramientaAAdherir plomero = plomero {herramienta =  herramientaAAdherir : herramienta plomero}

--Punto 5
repacionComplicada :: Reparaciones -> Bool
repacionComplicada reparacion = longitudDeDescripcion reparacion > 100 && esUnGrito (descripcion reparacion)
-- Poder usar isUpper ver despues

longitudDeDescripcion :: Reparaciones -> Int 
longitudDeDescripcion  = length . descripcion  

esUnGrito :: String -> Bool
esUnGrito (x:_) = esMayuscula x
esUnGrito _ = False

esMayuscula :: Char -> Bool
esMayuscula letra = letra `elem` ['A'..'Z']

presupuestoReparacion :: Reparaciones -> Int 
presupuestoReparacion = (3 *). longitudDeDescripcion

--Punto 6 
hacerReparacion :: Reparaciones -> Plomero -> Plomero
hacerReparacion  reparacion plomero 
    | sePuedeReparar (requerimiento reparacion) plomero  = (variarDinero (precioACobrar reparacion) .accionDelPlomero  reparacion )plomero 
    | otherwise = variarDinero 100 plomero

sePuedeReparar :: Herramienta -> Plomero -> Bool
sePuedeReparar herramienta  = tieneUnaHerramienta (denominacion herramienta) 

agregarReparacion :: Reparaciones -> Plomero -> Plomero
agregarReparacion reparacionAAgregr plomero = plomero { historial = reparacionAAgregr : historial plomero}

--Se puede mejorara
accionDelPlomero :: Reparaciones -> Plomero -> Plomero 
accionDelPlomero reparacion plomero 
    | esMalvadoElPlomero plomero    =  agregarHerramienta destornillador plomero 
    | repacionComplicada reparacion =  perderTodasHerramientas  plomero
    | otherwise                     =  perderHerramientas  plomero 

--Se puede mejorara
perderHerramientas :: Plomero -> Plomero
perderHerramientas plomero = plomero {herramienta = drop 1 (herramienta plomero)} 

perderTodasHerramientas :: Plomero -> Plomero
perderTodasHerramientas plomero = plomero {herramienta = []}

--Punto 7 
jornadaDeTrabajo :: [Reparaciones] -> Plomero -> Plomero
jornadaDeTrabajo reparaciones plomero = foldl (flip hacerReparacion) plomero reparaciones
-- 20: 12
--Retomado 22:50
--Punto 8 
type Criterio a = Plomero -> a

maximoTrabajadorPorCriterio :: (Ord a) => Criterio a -> [Plomero] -> Plomero 
maximoTrabajadorPorCriterio _ [x] = x
maximoTrabajadorPorCriterio criterio (x:y:xs)
    | criterio x < criterio y = maximoTrabajadorPorCriterio criterio (y:xs)
    | otherwise               = maximoTrabajadorPorCriterio criterio (x:xs)


empleadoMasTrabajado ::[Reparaciones] -> [Plomero]  -> Plomero
empleadoMasTrabajado  reparaciones  plomeros =  maximoTrabajadorPorCriterio (length . historial)  (map (jornadaDeTrabajo reparaciones) plomeros)
--23:03
--23:44

empleadoMasDinero :: [Reparaciones] -> [Plomero]  -> Plomero
empleadoMasDinero  reparaciones =  maximoTrabajadorPorCriterio dinero  . map (jornadaDeTrabajo reparaciones)

empleadoMasInvirtio :: [Reparaciones] -> [Plomero] -> Plomero 
empleadoMasInvirtio  reparaciones  =  maximoTrabajadorPorCriterio sumarHerramientas  . map (jornadaDeTrabajo reparaciones)

sumarHerramientas :: Plomero -> Float
sumarHerramientas  plomero = sum (map precio (herramienta plomero))
--00:22 Terminado