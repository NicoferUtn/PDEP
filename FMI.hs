import Text.Show.Functions
import Data.List(genericLength)

--Iniciado 01/06/2024 15:50

data Pais = Pais {
    ingresoPerCapita :: Int,
    publico :: Int,
    privado :: Int,
    recursos :: [Recursos],
    deuda :: Int
}
type Recursos = String
type Receta = Pais -> Pais

--Punto 1
nambia = Pais 4140 400000 650000 ["Mineria", "Ecoturismo"] 50000000

--Punto 2
prestarDinero :: Receta
prestarDinero pais = pais{ deuda = deuda pais * div 150 100 }

reducirActivosPerCapita :: Receta
reducirActivosPerCapita pais
    | publico pais > 100 = variarIngresoPerCapita 20 pais
    | otherwise          = variarIngresoPerCapita 15 pais

variarIngresoPerCapita :: Int -> Receta
variarIngresoPerCapita porcentaje pais = pais {ingresoPerCapita = ingresoPerCapita pais * div porcentaje 100}

explotarRecursoNatural :: Receta
explotarRecursoNatural = variarDeuda (-2000000). eliminarRecursos

eliminarRecursos :: Receta
eliminarRecursos pais = pais {recursos = drop 1 (recursos pais)}

variarDeuda :: Int -> Receta
variarDeuda valorPrestado pais = pais {deuda = max 0 (deuda pais + valorPrestado )} --No creo que el FMI le deba dinero a los Paises JJAJA 

blindaje :: Receta
blindaje pais = variarTrabajadores 500 . flip variarDeuda pais .calcularPBI $ pais

calcularPBI :: Pais -> Int 
calcularPBI (Pais ingresoPerCapita publico privado recursos deuda) = ingresoPerCapita * (publico + privado)

variarTrabajadores :: Int -> Receta 
variarTrabajadores trabajadores pais = pais {publico = max 0 (publico pais - trabajadores )}

--Punto 3 Ejemplo de Receta (Esto pide el punto)
recetaAAplicarAPais :: Receta
recetaAAplicarAPais  = agregarRecursoNatural "Mineria" . prestarDinero

agregarRecursoNatural :: Recursos -> Receta
agregarRecursoNatural recursoAAgregar pais = pais {recursos = recursoAAgregar : recursos pais }

--Punto 4
paisesAZafar :: Pais -> Bool
paisesAZafar  = perteneceElRecurso "Petroleo" . recursos

perteneceElRecurso :: String -> [Recursos] -> Bool
perteneceElRecurso recurso = any (\rec -> recurso == rec)

deudaTotalFMI :: [Pais] -> Int
deudaTotalFMI  = sum . deudaDeCadaPais 

deudaDeCadaPais :: [Pais] -> [Int]
deudaDeCadaPais = map deuda

--Punto 5 
esCreciente :: (Num a, Ord a) => [a] -> Bool
esCreciente [] = False
esCreciente [_] = True
esCreciente (x1:x2:xs) = x1 < x2 && esCreciente (x2:xs)


estaOredenado :: Pais -> [Receta] -> Bool
estaOredenado pais  =  esCreciente . map (\receta -> calcularPBI (receta pais))   

--Hora terminada 17:17