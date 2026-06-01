module Spec where
import PdePreludat
import Library
import Test.Hspec

platoFideos :: Plato
platoFideos = Plato 5 [Componente "Fideos" 200]

platoEnsalada :: Plato
platoEnsalada = Plato 4 [Componente "Lechuga" 50, Componente "Tomate" 50]

platoPesado :: Plato
platoPesado = Plato 8 [Componente "Sal" 3, Componente "Carne" 100, Componente "Cebolla" 5]

platoRival :: Plato
platoRival = Plato 4 [Componente "Roca" 500]

rivalPepe :: Participante
rivalPepe = Participante "Rival" [] platoRival

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2

  describe "Parte A - Trucos de Cocina" $ do
    it "endulzar agrega el componente 'Azucar' con el peso indicado al principio de la lista" $ do
      (ingrediente . head . componentes) (endulzar 10 platoFideos) `shouldBe` "Azucar"

    it "duplicarPorcion multiplica el peso de todos los componentes por 2" $ do
      (peso . head . componentes) (duplicarPorcion platoFideos) `shouldBe` 400

    it "simplificar le baja la dificultad a 5 a un plato complejo" $ do
      dificultad (simplificar platoPepe) `shouldBe` 5
      
    it "simplificar quita los componentes de menos de 10g en un plato complejo" $ do
      cantidadComponentes (simplificar platoPepe) `shouldBe` 0

    it "simplificar no hace nada en un plato que NO es complejo" $ do
      dificultad (simplificar platoFideos) `shouldBe` 5

  describe "Parte A - Consultas sobre Platos" $ do
    it "esVegano da True si no tiene carne, huevo ni lácteo" $ do
      esVegano platoEnsalada `shouldBe` True

    it "esVegano da False si tiene carne" $ do
      esVegano platoPesado `shouldBe` False

    it "esSinTacc da False si tiene Harina" $ do
      esSinTacc platoPepe `shouldBe` False

    it "esComplejo da True si tiene más de 5 componentes y dificultad mayor a 7" $ do
      esComplejo platoPepe `shouldBe` True

    it "noAptoHipertension da True si tiene un componente llamado Sal con más de 2 gramos" $ do
      noAptoHipertension platoPepe `shouldBe` True
      noAptoHipertension platoPesado `shouldBe` True

  describe "Parte B - Modelo de Datos" $ do
    it "La especialidad de Pepe Ronccino es un plato complejo" $ do
      esComplejo (especialidad pepeRonccino) `shouldBe` True

  describe "Parte C - Torneo y Evaluaciones" $ do
    it "esMejorQue da True si el plato tiene estrictamente más dificultad y menos peso total" $ do
      let platoPro = Plato 10 [Componente "Magia" 50]
      let platoNoob = Plato 5 [Componente "Roca" 100]
      esMejorQue platoPro platoNoob `shouldBe` True

    it "esMejorQue da False si el plato tiene más dificultad pero también es más pesado" $ do
      let platoPesadote = Plato 10 [Componente "Piedra" 1000]
      let platoLiviano = Plato 5 [Componente "Pluma" 10]
      esMejorQue platoPesadote platoLiviano `shouldBe` False

    it "cocinar aplica correctamente los trucos y devuelve un plato diferente al original" $ do
      dificultad (cocinar pepeRonccino) `shouldBe` 5

    it "participanteEstrella devuelve al ganador de una lista de participantes" $ do
      nombre (participanteEstrella [pepeRonccino, rivalPepe]) `shouldBe` "Pepe Ronccino"