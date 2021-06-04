module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

tripulante1 = UnTripulante 60
tripulante2 = UnTripulante 30
tripulante3 = UnTripulante 0


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
  describe "tripulantes" $ do
    it "enfrento a un esqueleto estando en las buenas" $ do
      enfrentarEsqueleto tripulante1 `shouldBe` UnTripulante 50


escribime :: Expectation
escribime = implementame

