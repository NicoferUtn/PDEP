import Text.Show.Functions
import Data.List(genericLength)
import Data.Char (isUpper)


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
type Criterio a = Plomero -> a
type Evento estructura = estructura -> estructura --Para que se entienda envento de que

llaveInglesa = Herramienta "Llave Inglesa" 200 Hierro
destornillador = Herramienta "Destornillador" 0 Plastico
--Punto 1 
mario = Plomero "Mario" [Herramienta "Llave inglesa" 200  Hierro, Herramienta "Martillo" 20 Madera] [] 1200 
wario = Plomero "Wario" llavesInfinitas [] 0.50

filtracionAgua = Reparaciones "Filtracion Agua" llaveInglesa

llavesInfinitas :: [Herramienta]
llavesInfinitas = map (\precioHerramienta -> Herramienta "llaveFrancesa" precioHerramienta Hierro) [1..] -- x nombre de mierda > precioHerramienta

--Punto 2 a.
tieneUnaHerramienta :: String -> Plomero -> Bool
tieneUnaHerramienta nombreHerramienta  = any (\herramientaUnica -> nombreHerramienta == denominacion herramientaUnica ) . herramienta  --Se puede mejorar el nombre pero no se me ocurre algo ¿Objeto? ¿herramientaUnica?

--Punto 2 b
esMalvadoElPlomero :: Plomero -> Bool
esMalvadoElPlomero plomero = take 2 (nombre plomero) == "Wa" 

--Punto 2 c
puedeComprarHerramienta :: Plomero -> Herramienta -> Bool
puedeComprarHerramienta plomero herramienta = dinero plomero <= precio herramienta --Me confundi habia puesto == y era <=

--Punto 3
esBuenaLaHerramienta :: Herramienta -> Bool
esBuenaLaHerramienta (Herramienta "Martillo" _ Madera ) = True
esBuenaLaHerramienta (Herramienta "Martillo" _ Goma) = True
esBuenaLaHerramienta (Herramienta _ _ Hierro) = True 
esBuenaLaHerramienta (Herramienta _ precio _) = precio > 10000

--Punto 4 
comprarUnaHerramienta :: Plomero -> Herramienta -> Plomero
comprarUnaHerramienta plomero herramienta 
    | puedeComprarHerramienta plomero herramienta = agregarHerramienta herramienta . variarDinero (-precio herramienta) $ plomero --Cambiar paretesis por $
    | otherwise = plomero 


variarDinero :: Float -> Evento Plomero
variarDinero precioAVariar plomero = plomero {dinero = dinero plomero + precioAVariar}

agregarHerramienta :: Herramienta -> Evento Plomero
agregarHerramienta herramientaAAdherir plomero = plomero {herramienta =  herramientaAAdherir : herramienta plomero}

--Punto 5
repacionComplicada :: Reparaciones -> Bool
repacionComplicada reparacion = longitudDeDescripcion reparacion > 100 && esUnGrito (descripcion reparacion)


longitudDeDescripcion :: Reparaciones -> Int 
longitudDeDescripcion  = length . descripcion  

esUnGrito :: String -> Bool
esUnGrito (letra:_) = isUpper letra --Cambiar a isUpper y x nombre de mierda > letra
esUnGrito _ = False

presupuestoReparacion :: Reparaciones -> Int 
presupuestoReparacion = (3 *) . longitudDeDescripcion

--Punto 6 
hacerReparacion :: Reparaciones -> Evento Plomero
hacerReparacion  reparacion plomero 
    | sePuedeReparar (requerimiento reparacion) plomero  = agregarReparacion reparacion . variarDinero (precioACobrar reparacion) . accionDelPlomero  reparacion $ plomero --Me falto agregarla a la lista y el $
    | otherwise = variarDinero 100 plomero

sePuedeReparar :: Herramienta -> Plomero -> Bool
sePuedeReparar herramienta plomero = tieneUnaHerramienta (denominacion herramienta) plomero || (esMalvadoElPlomero plomero && tieneUnaHerramienta "Martillo" plomero) --Me falto agregar el caso del martillo y es malvado

agregarReparacion :: Reparaciones -> Evento Plomero --LO HABIA ECHO Y NO LO PUSE SOY UN BOLUDONNNN
agregarReparacion reparacionAAgregr plomero = plomero { historial = reparacionAAgregr : historial plomero}


accionDelPlomero :: Reparaciones -> Evento Plomero 
accionDelPlomero reparacion plomero 
    | esMalvadoElPlomero plomero    =  agregarHerramienta destornillador plomero 
    | repacionComplicada reparacion =  perderTodasHerramientas  plomero
    | otherwise                     =  perderHerramientas  plomero 


perderHerramientas :: Evento Plomero
perderHerramientas  = variarCajaDeHerramientas (drop 1) --Un poco de logica repetida

perderTodasHerramientas :: Evento Plomero
perderTodasHerramientas  = variarCajaDeHerramientas (filter (not . esBuenaLaHerramienta))  -- No habia pueso que Eran solo las herramientas buenas y tenia un poco de logica repetida

variarCajaDeHerramientas :: ([Herramienta] -> [Herramienta]) -> Evento Plomero
variarCajaDeHerramientas funcionAAplicar plomero = plomero {herramienta = funcionAAplicar (herramienta plomero)}

--Punto 7 
jornadaDeTrabajo :: [Reparaciones] -> Evento Plomero
jornadaDeTrabajo reparaciones plomero = foldl (flip hacerReparacion) plomero reparaciones
-- 20: 12
--Retomado 22:50
--Punto 8 


maximoTrabajadorPorCriterio :: (Ord a) => Criterio a -> [Plomero] -> Plomero  --Cambio de nombres a (x:y:xs) -> (plomeroA:plomeroB:restoDePlomeros) Creo que se entiende mejor
maximoTrabajadorPorCriterio _ [plomero] = plomero
maximoTrabajadorPorCriterio criterio (plomeroA:plomeroB:restoDePlomeros)
    | criterio plomeroA < criterio plomeroB = maximoTrabajadorPorCriterio criterio (plomeroB:restoDePlomeros)
    | otherwise               = maximoTrabajadorPorCriterio criterio (plomeroA:restoDePlomeros)

plomerosDespuesDeLaJornada :: [Reparaciones] -> [Plomero] -> [Plomero]
plomerosDespuesDeLaJornada reparaciones = map (jornadaDeTrabajo reparaciones)

empleadoConMasTrabajos ::[Reparaciones] -> [Plomero]  -> Plomero --Nombre de mierda tenia "empleadosMasTabajados" > empleadoConMasTrabajos
empleadoConMasTrabajos  reparaciones  =  maximoTrabajadorPorCriterio (length . historial) . plomerosDespuesDeLaJornada reparaciones  --Cambiar map por funcion generica
--23:03
--23:44

empleadoConMasDinero :: [Reparaciones] -> [Plomero]  -> Plomero
empleadoConMasDinero  reparaciones =  maximoTrabajadorPorCriterio dinero  . plomerosDespuesDeLaJornada reparaciones --Cambiar map por funcion generica

empleadoConMasInvirtio :: [Reparaciones] -> [Plomero] -> Plomero 
empleadoConMasInvirtio  reparaciones  =  maximoTrabajadorPorCriterio sumarHerramientas  . plomerosDespuesDeLaJornada reparaciones --Cambiar map por funcion generica


sumarHerramientas :: Plomero -> Float
sumarHerramientas  plomero = sum (map precio (herramienta plomero))
--00:22 Terminado
