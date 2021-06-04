module Library where
import PdePreludat

--Tripulantes
data Tripulante = UnTripulante {
    energia::Number
} deriving (Show,Eq)

enfrentarEsqueleto :: Tripulante -> Tripulante
enfrentarEsqueleto tripulante
    |estaEnLasUltimas tripulante = perderEnergia 20 tripulante
    |otherwise = perderEnergia 10 tripulante

estaEnLasUltimas :: Tripulante -> Bool
estaEnLasUltimas (UnTripulante energia) = energia < 50

perderEnergia :: Number -> Tripulante -> Tripulante
perderEnergia valor (UnTripulante energia)
    |siLlegaACeroEnergia energia valor =  UnTripulante 0
    |otherwise = UnTripulante (energia-valor)

siLlegaACeroEnergia :: Ord a => a -> a -> Bool
siLlegaACeroEnergia energia valor = energia<=valor

transportarCarga :: Number -> Tripulante -> Tripulante
transportarCarga peso tripulante = perderEnergia peso tripulante

beberGrog :: Tripulante -> Tripulante
beberGrog tripulante = aumentarEnergia 20 tripulante

aumentarEnergia :: Number -> Tripulante -> Tripulante
aumentarEnergia valor (UnTripulante energia) = UnTripulante (energia+valor)

tripulanteEstaMuerto :: Tripulante -> Bool
tripulanteEstaMuerto (UnTripulante energia) = energia==0

obtenerEnergia :: Tripulante -> Number
obtenerEnergia (UnTripulante energia) = energia

-- Barcos

data Barco = UnBarco{
    tamanio::Number,
    tripulantes::[Tripulante],
    oro::Number,
    tablones::Number,
    balas::Number
} deriving (Show,Eq)

--(UnBarco tamanio tripulantes oro tablones balas)
galeon :: [Tripulante] -> Number -> Number -> Number -> Barco
galeon = UnBarco 150

bergantin :: [Tripulante] -> Number -> Number -> Number -> Barco
bergantin = UnBarco 100

balandro :: [Tripulante] -> Number -> Number -> Number -> Barco
balandro = UnBarco 50

esBarcoFantasma :: Barco -> Bool
esBarcoFantasma barco = noTieneTripulacion barco || tieneTodosTripulantesMuertos barco

noTieneTripulacion :: Barco -> Bool
noTieneTripulacion (UnBarco _ tripulantes _ _ _) = tripulantes==[]

tieneTodosTripulantesMuertos :: Barco -> Bool
tieneTodosTripulantesMuertos (UnBarco _ tripulantes _ _ _) = (todosMuertos . map tripulanteEstaMuerto) tripulantes

todosMuertos :: [Bool] -> Bool
todosMuertos tripulantes = all (==estarMuerto) tripulantes

estarMuerto :: Bool
estarMuerto=True

llenarBarco :: Barco -> Barco
llenarBarco barco = (llenarConOro . balasPorPersona) barco

balasPorPersona :: Barco -> Barco
balasPorPersona (UnBarco tamanio tripulantes oro tablones balas) = UnBarco tamanio tripulantes oro tablones (tresPorTripulante tripulantes)

tresPorTripulante :: [Tripulante] -> Number
tresPorTripulante tripulantes = 3 * length tripulantes

llenarConOro :: Barco -> Barco
llenarConOro (UnBarco tamanio tripulantes oro tablones balas) = UnBarco tamanio tripulantes (7*tamanio) tablones balas

--Enfrentamientos

enfrentamiento :: Barco -> Barco -> Barco
enfrentamiento barcoenemigo barco
    |tamañoBarco barco > tamañoBarco barcoenemigo = ganaMasSegun balasBarco barco barcoenemigo
    |tamañoBarco barco == tamañoBarco barcoenemigo = ganaMasSegun tablonesBarco barco barcoenemigo
    |otherwise = ganaMasSegun tripulantesConVida barco barcoenemigo

tamañoBarco :: Barco -> Number
tamañoBarco (UnBarco tamanio _ _ _ _) = tamanio

tablonesBarco :: Barco -> Number
tablonesBarco (UnBarco _ _ _ tablones _) = tablones

balasBarco :: Barco -> Number
balasBarco (UnBarco _ _ _ _ balas) = balas

oroBarco :: Barco -> Number
oroBarco (UnBarco _ _ oro _ _) = oro

tripulantesBarco :: Barco -> [Tripulante]
tripulantesBarco (UnBarco _ tripulantes _ _ _) = tripulantes

--tripulantesVivos tripulantes = (filter (/=0) . map obtenerEnergia) tripulantes
data Vida = Vivo | Muerto deriving (Show,Eq)

tripulantesVivos :: [Tripulante] -> [Tripulante]
tripulantesVivos [] = []
tripulantesVivos (tripulante:tripulantes)
    |tripulanteEstaMuerto tripulante = tripulantesVivos tripulantes
    |otherwise = tripulante:tripulantesVivos tripulantes

tripulantesMuertos :: [Tripulante] -> [Tripulante]
tripulantesMuertos [] = []
tripulantesMuertos (tripulante:tripulantes)
    |tripulanteEstaMuerto tripulante = tripulante:tripulantesVivos tripulantes
    |otherwise = tripulantesVivos tripulantes


tripulantesConVida :: Barco -> Number
tripulantesConVida (UnBarco _ tripulantes _ _ _) = (length . tripulantesVivos) tripulantes


ganaMasSegun :: Ord a => (Barco -> a) -> Barco -> Barco -> Barco
ganaMasSegun frecurso barco barcoenemigo
    |frecurso barco >= frecurso barcoenemigo = ganarEnfrentamiento barco barcoenemigo
    |otherwise = perderEnfrentamiento barco


--Si el barco pierde contra su oponente se queda sin recursos. En caso contrario, se lleva consigo todos los recursos del otro barco 
ganarEnfrentamiento :: Barco -> Barco -> Barco
ganarEnfrentamiento (UnBarco tamanio tripulantes oro tablones balas) barcoperdedor = UnBarco tamanio tripulantes (oro+oroBarco barcoperdedor) (tablones+tablonesBarco barcoperdedor) (balas+balasBarco barcoperdedor)

perderEnfrentamiento :: Barco -> Barco
perderEnfrentamiento (UnBarco tamanio tripulantes _ _ _) = UnBarco tamanio tripulantes 0 0 0

--Sucesos

tripulantesCambiados :: [Tripulante] -> Barco -> Barco
tripulantesCambiados tripulantesnuevos (UnBarco tamanio tripulantes oro tablones balas) = UnBarco tamanio tripulantesnuevos oro tablones balas

embarcarTesoro :: Number -> Barco -> Barco
embarcarTesoro peso (UnBarco tamanio tripulantes oro tablones balas) = UnBarco tamanio (distribuirTesoro peso tripulantes) (oro+peso) tablones balas


distribuirTesoro :: Number -> [Tripulante] -> [Tripulante]
distribuirTesoro peso tripulantes = (map (transportarCarga (div peso (tripulantesConVida (UnBarco 1 tripulantes 1 1 1)))) . tripulantesVivos) tripulantes -- puse 1s porque tengo que mandar un barco

--enfrentarAUnBarco seria igual a la funcion "enfrentamiento" definida con anterioridad

cargamentoDeGrog :: Barco -> Barco
cargamentoDeGrog barco = tripulantesCambiados (bebidoCincoGrogs barco) barco

bebidoCincoGrogs :: Barco -> [Tripulante]
bebidoCincoGrogs barco = (map(beberGrog.beberGrog.beberGrog.beberGrog.beberGrog) . tripulantesBarco) barco

ejercitoDeEsqueletos :: Number -> Barco -> Barco
ejercitoDeEsqueletos esqueletos barco = tripulantesCambiados (listaNuevaEjercitoEsqueletos esqueletos barco) barco

primerTripulanteVivo :: Barco -> Tripulante
primerTripulanteVivo = head . tripulantesVivos . tripulantesBarco

listaNuevaEjercitoEsqueletos :: Number -> Barco -> [Tripulante]
listaNuevaEjercitoEsqueletos esqueletos barco = (lucharEsqueletos esqueletos . primerTripulanteVivo) barco : filter (/=primerTripulanteVivo barco) (tripulantesBarco barco)

lucharEsqueletos :: Number -> Tripulante -> Tripulante
lucharEsqueletos 0 tripulante = tripulante
lucharEsqueletos esqueletos tripulante = lucharEsqueletos (esqueletos-1) (enfrentarEsqueleto tripulante)

tiendaDeGrog :: Barco -> Barco
tiendaDeGrog barco
    | esBarcoFantasma barco && oroBarco barco>=30 =  aplicarPrimerMuerto barco
    | otherwise = barco

primerMuerto :: [Tripulante] -> Tripulante
primerMuerto = head . tripulantesMuertos

aplicarPrimerMuerto :: Barco -> Barco
aplicarPrimerMuerto (UnBarco tamanio tripulantes oro tablones balas) = UnBarco tamanio ((beberGrog . primerMuerto) tripulantes:filter (==primerMuerto tripulantes) tripulantes) (oro-30) tablones balas

--Travesias

fuerteDeLosCondenados :: [Barco -> Barco]
fuerteDeLosCondenados=[ejercitoDeEsqueletos 100, tiendaDeGrog, embarcarTesoro 30]

mandarBarco :: [Barco -> Barco] -> Barco -> Barco
mandarBarco travesia barco
    |esBarcoFantasma (foldr ($) barco travesia) = UnBarco (tamañoBarco barco) [] (oroBarco barco) (tablonesBarco barco) (balasBarco barco)
    |otherwise = cobrarRecompensa travesia (foldr ($) barco travesia)

cobrarRecompensa :: p -> Barco -> Barco
cobrarRecompensa fuerteDeLosCondenados barco = agregarOro 50 barco
cobrarRecompensa travesiaDeFlameHeart barco = agregarOro (((4*) . length . tripulantesVivos . tripulantesBarco) barco) barco
cobrarRecompensa girita barco = agregarOro (2*oroBarco barco) barco

agregarOro :: Number -> Barco -> Barco
agregarOro cant (UnBarco tamanio tripulantes oro tablones balas) = (UnBarco tamanio tripulantes (oro+cant) tablones balas)
travesiaDeFlameHeart :: [Barco -> Barco]
travesiaDeFlameHeart = [enfrentamiento (galeon [(UnTripulante 30),(UnTripulante 30),(UnTripulante 30),(UnTripulante 30)] 50 50 50), enfrentamiento (bergantin [(UnTripulante 10),(UnTripulante 10),(UnTripulante 10)] 30 30 30),cargamentoDeGrog,embarcarTesoro 150]   --no aclara cuanto oro tiene asi que puse el mismo valor que el resto de cosas


girita :: [Barco -> Barco]
girita = ([tiendaDeGrog,tiendaDeGrog,tiendaDeGrog,tiendaDeGrog,ejercitoDeEsqueletos 10])


