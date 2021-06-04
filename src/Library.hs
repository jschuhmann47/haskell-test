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

esBarcoFantasma barco = noTieneTripulacion barco || tripulantesMuertos barco

noTieneTripulacion (UnBarco _ tripulantes _ _ _) = tripulantes==[]

tripulantesMuertos (UnBarco _ tripulantes _ _ _) = (todosMuertos . map tripulanteEstaMuerto) tripulantes

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

tripulantesVivos tripulantes = (filter (/=0) . map obtenerEnergia) tripulantes

tripulantesConVida (UnBarco _ tripulantes _ _ _) = (length . tripulantesVivos) tripulantes

ganaMasSegun frecurso barco barcoenemigo
    |frecurso barco >= frecurso barcoenemigo = barcoLeGanoA barco barcoenemigo
    |otherwise = barcoLeGanoA barcoenemigo barco



barcoLeGanoA barco barcoperdedor = 
