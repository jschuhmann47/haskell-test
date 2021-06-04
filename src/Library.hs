module Library where
import PdePreludat

--Tripulantes
data Tripulante = UnTripulante {
    energia::Number
} deriving (Show,Eq)

enfrentarEsqueleto tripulante
    |estaEnLasUltimas tripulante = perderEnergia 20 tripulante
    |otherwise = perderEnergia 10 tripulante

estaEnLasUltimas (UnTripulante energia) = energia < 50

perderEnergia valor (UnTripulante energia)
    |siLlegaACeroEnergia energia valor =  UnTripulante 0
    |otherwise = UnTripulante (energia-valor)

siLlegaACeroEnergia energia valor = energia<=valor

transportarCarga peso tripulante = perderEnergia peso tripulante

beberGrog tripulante = aumentarEnergia 20 tripulante

aumentarEnergia valor (UnTripulante energia) = UnTripulante (energia+valor)

tripulanteEstaMuerto (UnTripulante energia) = energia==0

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
galeon = UnBarco 150

bergantin = UnBarco 100

balandro = UnBarco 50

esBarcoFantasma barco = noTieneTripulacion barco || tieneTripulantesMuertos barco

noTieneTripulacion (UnBarco _ tripulantes _ _ _) = tripulantes==[]

tieneTripulantesMuertos (UnBarco _ tripulantes _ _ _) = (todosMuertos . map tripulanteEstaMuerto) tripulantes

todosMuertos tripulantes = all (==estarMuerto) tripulantes

estarMuerto=True

llenarBarco barco = (llenarConOro . balasPorPersona) barco

balasPorPersona (UnBarco tamanio tripulantes oro tablones balas) = UnBarco tamanio tripulantes oro tablones (tresPorTripulante tripulantes)

tresPorTripulante tripulantes = 3 * length tripulantes

llenarConOro (UnBarco tamanio tripulantes oro tablones balas) = UnBarco tamanio tripulantes (7*tamanio) tablones balas

--Enfrentamientos

enfrentamiento barco barcoenemigo
    |tamañoBarco barco > tamañoBarco barcoenemigo = ganaMasSegun balasBarco barco barcoenemigo
    |tamañoBarco barco == tamañoBarco barcoenemigo = ganaMasSegun tablonesBarco barco barcoenemigo
    |otherwise = ganaMasSegun tripulantesConVida barco barcoenemigo

tamañoBarco (UnBarco tamanio _ _ _ _) = tamanio

tablonesBarco (UnBarco _ _ _ tablones _) = tablones

balasBarco (UnBarco _ _ _ _ balas) = balas

oroBarco (UnBarco _ _ oro _ _) = oro

tripulantesBarco (UnBarco _ tripulantes _ _ _) = tripulantes

--tripulantesVivos tripulantes = (filter (/=0) . map obtenerEnergia) tripulantes
data Vida = Vivo | Muerto deriving (Show,Eq)

tripulantesVivos [] = []
tripulantesVivos (tripulante:tripulantes)
    |tripulanteEstaMuerto tripulante = tripulantesVivos tripulantes
    |otherwise = tripulante:tripulantesVivos tripulantes

tripulantesMuertos [] = []
tripulantesMuertos (tripulante:tripulantes)
    |tripulanteEstaMuerto tripulante = tripulante:tripulantesVivos tripulantes
    |otherwise = tripulantesVivos tripulantes


tripulantesConVida (UnBarco _ tripulantes _ _ _) = (length . tripulantesVivos) tripulantes


ganaMasSegun frecurso barco barcoenemigo
    |frecurso barco >= frecurso barcoenemigo = ganarEnfrentamiento barco barcoenemigo
    |otherwise = perderEnfrentamiento barco


--Si el barco pierde contra su oponente se queda sin recursos. En caso contrario, se lleva consigo todos los recursos del otro barco 
ganarEnfrentamiento (UnBarco tamanio tripulantes oro tablones balas) barcoperdedor = UnBarco tamanio tripulantes (oro+oroBarco barcoperdedor) (tablones+tablonesBarco barcoperdedor) (balas+balasBarco barcoperdedor)

perderEnfrentamiento (UnBarco tamanio tripulantes _ _ _) = UnBarco tamanio tripulantes 0 0 0

--Sucesos

tripulantesCambiados tripulantesnuevos (UnBarco tamanio tripulantes oro tablones balas) = UnBarco tamanio tripulantesnuevos oro tablones balas

embarcarTesoro peso (UnBarco tamanio tripulantes oro tablones balas) = UnBarco tamanio (distribuirTesoro peso tripulantes) (oro+peso) tablones balas


distribuirTesoro peso tripulantes = (map (transportarCarga (div peso (tripulantesConVida (UnBarco 1 tripulantes 1 1 1)))) . tripulantesVivos) tripulantes -- puse 1s porque tengo que mandar un barco

--enfrentarAUnBarco seria igual a la funcion "enfrentamiento" definida con anterioridad

cargamentoDeGrog barco = tripulantesCambiados (bebidoCincoGrogs barco) barco

bebidoCincoGrogs barco = (map(beberGrog.beberGrog.beberGrog.beberGrog.beberGrog) . tripulantesBarco) barco

ejercitoDeEsqueletos esqueletos barco = (lucharEsqueletos esqueletos . head . tripulantesVivos . tripulantesBarco) barco

lucharEsqueletos esqueletos tripulante = take (esqueletos+1) (iterate enfrentarEsqueleto tripulante)

tiendaDeGrog barco
    | esBarcoFantasma barco && oroBarco barco>=30 =  aplicarPrimerMuerto barco

primerMuerto = head . tripulantesMuertos

aplicarPrimerMuerto (UnBarco tamanio tripulantes oro tablones balas) = ((beberGrog . primerMuerto) tripulantes):filter (==(primerMuerto tripulantes)) tripulantes




