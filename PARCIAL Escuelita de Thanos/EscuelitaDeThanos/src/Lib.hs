module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

doble :: Num a => a -> a
doble = (*2)

{--
Punto 1: (2 puntos) Modelar Personaje, Guantelete y Universo como tipos de dato e implementar el chasquido de un universo.

--}

data Guantelete = Guantelete {
    materialDelGuantelete :: String,
    gemasDelGuantelete :: [Gema]
}

data Personaje = Personaje {
    edadDelPersonaje :: Int,
    energiaDelPersonaje :: Int,
    habilidadesDelPersonaje :: [Habilidad],
    nombreDelPersonaje :: String,
    planetaDelPersonaje :: String

}

type Planeta = String
type Habilidad = String
type Universo = [Personaje]
type Gema = (Personaje->Personaje)

chasquidoDeUnUniverso :: Universo -> Universo
chasquidoDeUnUniverso unUniverso = take (mitadCantidadDePersonajes unUniverso) unUniverso

mitadCantidadDePersonajes :: Universo -> Int
mitadCantidadDePersonajes = flip div 2.length 


{--
Punto 2: (3 puntos) Resolver utilizando únicamente orden superior.
Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.
Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.

--}

edadDeViejo :: Int
edadDeViejo = 45

pocaHabilidad :: Int
pocaHabilidad = 1

esAptoParaPendex :: Universo -> Bool
esAptoParaPendex  = any ((<edadDeViejo).energiaDelPersonaje)  
--esAptoParaPendex unUniverso = any (=="knojniun").habilidadesDelPersonaje $  unUniverso

energiaTotalDelUniverso :: Universo -> Int
energiaTotalDelUniverso = sum.map energiaDelPersonaje.filter tieneHabilidadConsiderble 

tieneHabilidadConsiderble :: Personaje -> Bool
tieneHabilidadConsiderble = (>pocaHabilidad).length.habilidadesDelPersonaje


-- Punto 3: (3 puntos) Implementar las gemas del infinito, evitando lógica duplicada. 

quitarEnergiaAUnPersonaje :: Int -> Personaje-> Int
quitarEnergiaAUnPersonaje valorEnergia =  flip (-) valorEnergia.energiaDelPersonaje 

-- La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado.

laMente :: Int->Gema
laMente valorDado unPersonaje = unPersonaje{ energiaDelPersonaje = quitarEnergiaAUnPersonaje valorDado unPersonaje}

--El alma puede controlar el alma de nuestro oponente permitiéndole eliminar una habilidad en particular si es que la posee. 
--Además le quita 10 puntos de energía.

cantEnergiaPerdida :: Int
cantEnergiaPerdida = 10

elAlma :: Habilidad -> Gema
elAlma unaHabilidad unPersonaje 
                                | any (==unaHabilidad).habilidadesDelPersonaje $ unPersonaje = unPersonaje {habilidadesDelPersonaje= filter (/= unaHabilidad).habilidadesDelPersonaje $ unPersonaje , energiaDelPersonaje= quitarEnergiaAUnPersonaje cantEnergiaPerdida unPersonaje } 
                                | otherwise = unPersonaje {energiaDelPersonaje = quitarEnergiaAUnPersonaje cantEnergiaPerdida unPersonaje } 

--El espacio que permite transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía.

cantEnergiaParaTransportarse :: Int
cantEnergiaParaTransportarse = 20

elEspacio :: Planeta -> Gema
elEspacio unPlanetaX unPersonaje = unPersonaje {planetaDelPersonaje= unPlanetaX, energiaDelPersonaje= quitarEnergiaAUnPersonaje cantEnergiaParaTransportarse unPersonaje }


-- El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en caso contrario no le saca ninguna habilidad).


cantHabilidadesMinimaParaResistirElPoder :: Int
cantHabilidadesMinimaParaResistirElPoder = 3

careceDeHabilidadesNecesariasParaResistirElPoder :: Personaje -> Bool
careceDeHabilidadesNecesariasParaResistirElPoder =  (<cantHabilidadesMinimaParaResistirElPoder).length.habilidadesDelPersonaje

elPoder :: Gema 
elPoder unPersonaje 
                    | careceDeHabilidadesNecesariasParaResistirElPoder unPersonaje = unPersonaje{energiaDelPersonaje = 0, habilidadesDelPersonaje = []}
                    | otherwise = unPersonaje {energiaDelPersonaje=0}

{--
El tiempo que reduce a la mitad la edad de su oponente pero como no está permitido pelear con menores, 
no puede dejar la edad del oponente con menos de 18 años. Considerar la mitad entera, por ej:
si el oponente tiene 50 años, le quedarán 25. Si tiene 45, le quedarán 22 (por división entera). 
Si tiene 30 años, le deben quedar 18 en lugar de 15. También resta 50 puntos de energía.

--}

cantEnergiaParaDisminuirEdad :: Int
cantEnergiaParaDisminuirEdad = 50

edadMinimaDeMayores :: Int
edadMinimaDeMayores = 18

mitadEnteraEdad :: Personaje -> Int
mitadEnteraEdad = flip div 2.edadDelPersonaje 

elTiempo :: Gema
elTiempo unPersonaje
                    | (>edadMinimaDeMayores).mitadEnteraEdad $ unPersonaje  = unPersonaje {edadDelPersonaje = mitadEnteraEdad unPersonaje , energiaDelPersonaje = quitarEnergiaAUnPersonaje cantEnergiaParaDisminuirEdad unPersonaje}
                    | otherwise = unPersonaje {edadDelPersonaje = edadMinimaDeMayores, energiaDelPersonaje = quitarEnergiaAUnPersonaje cantEnergiaParaDisminuirEdad unPersonaje}

{--
La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival.
--}                    

laGemaLoca :: Gema->Gema
laGemaLoca unaGema = unaGema.unaGema  

{--
Punto 4: (1 punto) Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de “usar Mjolnir” y
la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.
--}

guanteleteDeGoma = Guantelete "Goma" [elTiempo,elAlma "usar Mjolnir", laGemaLoca (elAlma "programación en Haskell")]

{--
Punto 5: (2 puntos). No se puede utilizar recursividad. Generar la función utilizar  que dado una lista de gemas y 
un enemigo ejecuta el poder de cada una de las gemas que lo componen contra el personaje dado. 
Indicar cómo se produce el “efecto de lado” sobre la víctima.
--}

utilizar :: [Gema]->Personaje->Personaje
utilizar =flip (foldr  ($)) 
-- El efecto de lado en este caso hace que la semilla y lo devuelto por las gemas se mantengan siempre a la derecha mientras que 
-- las gemas se posicionan siempre a la izquierda. Esto permite tener una buena aplicacion de las gemas sobre los personajes
-- ya que se mantiene el orden _Gema $ Personaje_ 

{--
Punto 6: (2 puntos). Resolver utilizando recursividad. Definir la función gemaMasPoderosa que dado un guantelete y 
una persona obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima. 
--}


listaEfectosGemasSobreEnergia :: Guantelete->Personaje->[Int]
listaEfectosGemasSobreEnergia unGuantelete unPersonaje = (map (energiaDelPersonaje.flip ($) unPersonaje)).gemasDelGuantelete $ unGuantelete


gemaMasPoderosa :: Guantelete->Personaje->Gema
gemaMasPoderosa unGuantelete unPersonaje = encontrarGemaQueQuitaMasEnergia (gemasDelGuantelete  unGuantelete) (listaEfectosGemasSobreEnergia unGuantelete unPersonaje) unPersonaje

encontrarGemaQueQuitaMasEnergia :: [Gema]->[Int]->Personaje->Gema
encontrarGemaQueQuitaMasEnergia (cabezaListaGemas:colaListaGemas) listaEnergiasPostGemas unPersonaje 
                                                                                            | (==(energiaDelPersonaje.cabezaListaGemas $ unPersonaje)).minimum $ listaEnergiasPostGemas =  cabezaListaGemas
                                                                                            | otherwise = encontrarGemaQueQuitaMasEnergia colaListaGemas listaEnergiasPostGemas unPersonaje

{--
Punto 7: (1 punto) Dada la función generadora de gemas y un guantelete de locos:
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

Y la función 
usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
gemaMasPoderosa punisher guanteleteDeLocos
usoLasTresPrimerasGemas guanteleteDeLocos punisher


--}
-- La primera no se puede ejecutar porque es necesario tener la lista de gemas del guantelete completa para saber cuál de ellas
-- tiene mayor efecto sobre la energia del personaje. Como la lista es infinita la funcion gemaMasPoderosa nunca va a terminar de 
-- ejecutarse ante la imposibilidad de obtener el resultado del efecto de todas las gemas.

-- La segunda si se puede ejecutar ya que la funcion utilizar tomaría una lista finita de gemas debido a que solo se toman las 
-- primeras 3 de una lista infinita de gemas. Esto es posible porque no es necesario conocer todos los elementos de una lista 
-- para tomar los primeros n elementos de dicha lista (siendo n un numero natural y menor al tamaño de la lista)



usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas = utilizar . take 3. gemasDelGuantelete 