module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

tripulante1 :: Tripulante
tripulante1 = UnTripulante 60
tripulante2 :: Tripulante
tripulante2 = UnTripulante 30
tripulante3 :: Tripulante
tripulante3 = UnTripulante 0

barco1 :: Barco
barco1=UnBarco 100 [tripulante1,tripulante2] 10 5 20
barco2 :: Barco
barco2=UnBarco 150 [tripulante1,tripulante3] 10 10 10

correrTests :: IO ()
correrTests = hspec $ do
  describe "tripulantes" $ do
    it "enfrento a un esqueleto estando en las buenas" $ do
      enfrentarEsqueleto tripulante1 `shouldBe` UnTripulante 50
    it "enfrento a un esqueleto estando en las ultimas" $ do
      enfrentarEsqueleto tripulante2 `shouldBe` UnTripulante 10
    it "transportar una carga que me deje con energia" $ do
      transportarCarga 30 tripulante1 `shouldBe` UnTripulante 30
    it "transportar una carga que deje sin energia" $ do
      transportarCarga 40 tripulante2 `shouldBe` UnTripulante 0
    it "beber grog aumenta 20 de energia en cualquier persona viva" $ do
      beberGrog tripulante1 `shouldBe` UnTripulante 80
    it "beber grog en una persona muerta" $ do
      beberGrog tripulante3 `shouldBe` UnTripulante 20
    it "saber si esta muerta una persona viva deberia dar false" $ do
      tripulanteEstaMuerto tripulante1 `shouldBe` False
    it "saber si esta muerta una persona muerta deberia dar true" $ do
      tripulanteEstaMuerto tripulante3 `shouldBe` True
  describe "barcos" $ do
    it "uso barco fantasma en un barco con gente viva y deberia dar false" $ do
      esBarcoFantasma barco1 `shouldBe` False
    it "uso barco fantasma en un barco sin gente y deberia dar true" $ do
      esBarcoFantasma barco1{tripulantes=[]} `shouldBe` True
    it "uso barco fantasma en un barco con gente muerta y deberia dar true" $ do
      esBarcoFantasma barco1{tripulantes=[tripulante3]} `shouldBe` True
    it "lleno un barco" $ do
      llenarBarco barco1 `shouldBe` UnBarco 100 [tripulante1,tripulante2] 700 5 6
  describe "enfrentamientos" $ do
    it "Un barco de mayor tamaño lucha contra otro de menor tamaño y al tener igual cant de balas gana" $ do
      enfrentamiento barco1 barco2{balas=20}  `shouldBe` UnBarco 150 [tripulante1,tripulante3] 20 15 40
    it "Un barco de mayor tamaño lucha contra otro de menor tamaño y pierde por tener menos balas" $ do
      enfrentamiento barco1 barco2  `shouldBe` UnBarco 150 [tripulante1,tripulante3] 0 0 0
    it "un barco de menor tamaño lucha contra otro de mayor tamaño y gana por tener mas tripulantes vivos" $ do
      enfrentamiento barco2 barco1  `shouldBe` UnBarco 100 [tripulante1,tripulante2] 20 15 30
    it "un barco de menor tamaño lucha contra otro de mayor tamaño y pierde por tener menos gente viva" $ do
      enfrentamiento barco2 barco1{tripulantes=[]}  `shouldBe` UnBarco 100 [] 0 0 0
    it "barcos de igual tamaño luchan y el primero gana por tener mas tablones de madera" $ do
      enfrentamiento barco2 barco1{tamanio=150,tablones=15}  `shouldBe` UnBarco 150 [tripulante1,tripulante2] 20 25 30
    it "barcos de igual tamaño luchan pero el primero pierde por tener menos tablones de madera" $ do
      enfrentamiento barco2 barco1{tamanio=150} `shouldBe` UnBarco 150 [tripulante1,tripulante2] 0 0 0
  describe "sucesos" $ do
    it "embarco un tesoro y me da el resultado" $ do  
      embarcarTesoro 10 barco1 `shouldBe` UnBarco 100 [tripulante1{energia=55},tripulante2{energia=25}] 20 5 20
    it "encuentro un cargamento de grog y todos los tripulantes toman 5 grogs" $ do
      cargamentoDeGrog barco1 `shouldBe` UnBarco 100 [tripulante1{energia=160},tripulante2{energia=130}] 10 5 20
    it "enfrento a 3 esqueletos con un barco y el tripulante termina vivo" $ do
      ejercitoDeEsqueletos 3 barco1 `shouldBe` barco1{tripulantes=[tripulante1{energia=20},tripulante2{energia=30}]}
    it "enfrento a 5 esqueletos con un barco y el tripulante termina muerto" $ do
      ejercitoDeEsqueletos 5 barco1 `shouldBe` UnBarco 100 [tripulante1{energia=0},tripulante2{energia=30}] 10 5 20
    it "paso por una tienda de grog y si estan todos bien no compro" $ do
      tiendaDeGrog barco1 `shouldBe` barco1
    it "paso por una tienda de grog y hay uno herido, pero como no tengo el oro suficiente no compro" $ do
      tiendaDeGrog barco2 `shouldBe` barco2
    it "paso por una tienda de grog y hay uno herido, y como tengo el oro suficiente compro" $ do
      tiendaDeGrog barco2{oro=100} `shouldBe` barco2{tripulantes=[tripulante3{energia=20},tripulante1],oro=70}
  describe "travesias" $ do
    it "travesia de fuerte de los condenados" $ do
      mandarBarco fuerteDeLosCondenados barco1 `shouldBe` UnBarco 100 [tripulante1{energia=0},tripulante2{energia=0}] 10 5 20
    -- no me da el tiempo para los otros tests :(

escribime :: Expectation
escribime = implementame

