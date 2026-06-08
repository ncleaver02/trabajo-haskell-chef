module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Parte A--------------------------------------------------------------------------------------------------------------------------------
data Participante = Participante {
    nombre :: String,
    trucosDeCocina :: [Truco],
    especialidad :: Plato
} deriving (Show)

data Plato = Plato {
    dificultad :: Number,
    componentes :: [Componente]
} deriving (Show)

data Componente = Componente {
    ingrediente :: String,
    peso :: Number
} deriving (Show)

type Truco = Plato -> Plato

endulzar :: Number -> Truco
endulzar unPeso unPlato = agregarComponente (Componente "Azucar" unPeso) unPlato

agregarComponente :: Componente -> Truco
agregarComponente unComponente (Plato dificultad componentes) =  Plato dificultad $ unComponente : componentes


salar :: Number -> Truco
salar unPeso unPlato = agregarComponente (Componente "Sal" unPeso) unPlato


darSabor :: Number -> Number -> Truco
darSabor pesoAzucar pesoSal unPlato = endulzar pesoAzucar . salar pesoSal $ unPlato


duplicarPorcion :: Truco
duplicarPorcion (Plato dificultad componentes) = Plato dificultad $ map (multiplicarPorcion 2) componentes

multiplicarPorcion :: Number -> Componente -> Componente
multiplicarPorcion unNumero (Componente ingrediente peso) = Componente ingrediente $ peso * unNumero


simplificar :: Truco
simplificar unPlato 
   | cantidadComponentes unPlato > 5 && dificultad unPlato > 7 = cambiarDificultad 5 . quitarComponentesLigeros $ unPlato
   | otherwise = unPlato

cantidadComponentes :: Plato -> Number
cantidadComponentes (Plato _ componentes) = length componentes

cambiarDificultad :: Number -> Truco
cambiarDificultad unNumero (Plato _ componentes) = Plato unNumero componentes

quitarComponentesLigeros :: Truco
quitarComponentesLigeros (Plato dificultad componentes) = Plato dificultad $ filter (not . masLigeroQue 10) componentes

masLigeroQue :: Number -> Componente -> Bool
masLigeroQue unNumero (Componente _ peso) = peso < unNumero


esVegano :: Plato -> Bool
esVegano unPlato = not (tiene "Carne" unPlato) && not (tiene "Huevo" unPlato) && not (tiene "Lacteo" unPlato)

tiene :: String -> Plato -> Bool
tiene unIngrediente (Plato _ componentes) = any (esIngrediente unIngrediente) componentes

esIngrediente :: String -> Componente -> Bool
esIngrediente unIngredienteBuscado (Componente ingrediente _) = unIngredienteBuscado == ingrediente

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not (tiene "Harina" unPlato)


esComplejo :: Plato -> Bool
esComplejo unPlato = cantidadComponentes unPlato > 5 && dificultad unPlato > 7


noAptoHipertension :: Plato -> Bool
noAptoHipertension (Plato _ componentes) = any (masPesadoQue 2) . filter (esIngrediente "Sal") $ componentes

masPesadoQue :: Number -> Componente -> Bool
masPesadoQue unNumero (Componente _ peso) = peso > unNumero

-- Parte B--------------------------------------------------------------------------------------------------------------------------------

pepeRonccino :: Participante
pepeRonccino = Participante {
    nombre = "Pepe Ronccino",
    trucosDeCocina = [darSabor 5 2, simplificar, duplicarPorcion],
    especialidad = platoPepe
}


platoPepe :: Plato
platoPepe = Plato {
    dificultad = 8,
    componentes = [Componente "Sal" 4, Componente "Azucar" 2, Componente "Carne" 5, Componente "Harina" 9, Componente "Lacteo" 4, Componente "Huevo" 1]
}

-- Parte C--------------------------------------------------------------------------------------------------------------------------------

cocinar :: Participante -> Plato
cocinar (Participante _ trucosDeCocina especialidad) = foldl aplicarTruco (especialidad) (trucosDeCocina)

aplicarTruco :: Plato -> Truco -> Plato
aplicarTruco unPlato unTruco = unTruco unPlato


esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = dificultad unPlato > dificultad otroPlato && (sumaPesos . componentes) unPlato < (sumaPesos . componentes) otroPlato

sumaPesos :: [Componente] -> Number
sumaPesos unosComponentes = sum $ map peso unosComponentes


participanteEstrella :: [Participante] -> Participante
participanteEstrella listaParticipantes = foldl1 mejorParticipante listaParticipantes

mejorParticipante :: Participante -> Participante -> Participante
mejorParticipante unParticipante otroParticipante
    | esMejorQue (cocinar unParticipante) (cocinar otroParticipante) = unParticipante
    | otherwise = otroParticipante

-- Parte D--------------------------------------------------------------------------------------------------------------------------------

-- platinum :: Plato
-- platinum = Plato {
-- dificultad = 10,
-- componentes = [(componente "ingrediente 1" 1), (componente "ingrediente 2" 2), (componente "ingrediente 3" 3), ..] 


-- Trucos: endulzar: Funciona. Esto porque la funcion agrega el azucar como primer componente de la lista asi que haskell no necesita compilar la lista entera.

-- Salar: Funciona. idem endulzar para sal.

-- DarSabor: Funciona. Agrega al principio de la lista los 2 componentes, asi que funciona por la misma razon que funciona endulzar y salar.

-- DuplicarPorcion: Funciona. Esto es debido a la naturaleza perezosa de Haskell, el cual no multiplica todas las porciones en la lista sino que agrega un multiplicador que va a tener efecto si se usa esta funcion con otra mas.

-- Simplificar: No funciona. Esto porque en la parte de revisar la cantidad de componentes se uso length, por lo que tendria que revisar la cantidad total, que es infinita, y no va a poder.

-- EsVegano: No funciona. Esto se debe a la funcion tiene, que usa any, y a como esta hecho el platinum, ya que este no contiene nada de lo que busca la funcion, seguira por siempre.

-- EsSinTacc: No funciona. Idem esVegano.

-- EsComplejo: No funciona. Misma razon que no funciona simplificar, por el cantidadComponentes y el length.

-- NoAptoHipertension: No funciona. Porque con la funcion filter va a intentar recorrer toda la lista para encontrar sales, por lo tanto no sera posible debido a que nunca terminara de buscar.

-- esMejorQue: No funciona. Esto es debido a que aunque si puede comparar dificultades, no va a poder comparar los pesos totales, ya que usando el sum se recorre toda la lista para sumar los pesos, lo cual nunca va a terminar de hacer.

