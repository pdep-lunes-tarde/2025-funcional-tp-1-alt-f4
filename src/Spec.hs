module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
-- Si alguna suit de tests tiene "focus" adelante, solo se va a correr esa.
-- Asi que, para ir probando los puntos, agreguen focus a los demas, o saquenselo a todos:
  focus suiteDeTestsDeParteI
  suiteDeTestsDeParteIBonus
  suiteDeTestsDeParteII
  
suiteDeTestsDeParteI =
  describe "Parte I: Numeros" $ do

    describe "siguiente" $ do
      it "el siguiente de un numero es el numero + 1" $ do
        siguiente (-1) `shouldBe` 0
        siguiente 0 `shouldBe` 1
        siguiente 1 `shouldBe` 2

    describe "inversa" $ do
      it "la inversa de 1 es 1" $ do
        inversa 1 `shouldBe` 1
      it "la inversa de cualquier numero es el resultado de dividir 1 por ese numero" $ do
        inversa 4 `shouldBe` 0.25
        inversa 0.25 `shouldBe` 4

    describe "esPositivo" $ do
      it "es verdad para los numeros mayores a 0" $ do
        esPositivo 1 `shouldBe` True
      it "es falso para los numeros menores a 0" $ do
        esPositivo (-1) `shouldBe` False
      it "es falso para el 0" $ do
        esPositivo 0 `shouldBe` False

suiteDeTestsDeParteIBonus =
  describe "Parte 1 Bonus" $ do
    it "el perimetro de un circulo es 2 * pi * radio" $ do
      perimetroCirculo 2.5 `shouldBeEqualUpTo2Decimals` 15.70796
    it "el perimetro de un cuadrado es el lado x 4" $ do
      perimetroCuadrado 3 `shouldBe` 12
    it "la superficie de un cuadrado es el lado al cuadrado" $ do
      superficieCuadrado 3 `shouldBe` 9
    it "la superficie de un cubo es el area de una cara por la cantidad de caras (6)" $ do
      superficieCubo 3 `shouldBe` 54
    it "la superficie de un cilindro es el area de las tapas por el area de la pared del cilindro" $ do
      superficieCilindro 2 4 `shouldBeEqualUpTo2Decimals` 75.39822

suiteDeTestsDeParteII =
  describe "Parte 2: Temperaturas" $ do
    describe "celsiusAFahrenheit" $ do
      it "pasa una temperatura de celsius a fahrenheit" $ do
        celsiusAFahrenheit 8 `shouldBeEqualUpTo2Decimals` 46.4

    describe "fahrenheitACelsius" $ do
      it "pasa una temperatura de fahrenheit a celsius" $ do
        fahrenheitACelsius 46.4 `shouldBeEqualUpTo2Decimals` 8

    describe "fahrenheitACelsius y celsiusAFahrenheit son inversas" $ do
      it "convertir un valor en celsius a fahrenheit y luego volver a convertir a celsius retorna el valor original" $ do
        fahrenheitACelsius (celsiusAFahrenheit 100) `shouldBe` 100 
      it "convertir un valor en fahrenheit a celsius y luego volver a convertir a fahrenheit retorna el valor original" $ do
        celsiusAFahrenheit (fahrenheitACelsius 100) `shouldBe` 100      

    describe "haceFrioCelsius" $ do
        it "Es verdad cuando hacen menos de 8 grados celsius" $ do
          shouldBe (haceFrioCelsius 5) True
        it "cuando la temperatura es mayor a 8 hace frio" $ do
           shouldBe (haceFrioCelsius 9) False
        it "cuando hacen exactamente 8 grados hace frio" $ do
            shouldBe (haceFrioCelsius 8) True

    describe "haceFrioFahrenheit" $ do
      it "la temperatura ya en celsius es mayor a 8" $ do
        pendingWith "Pensar un caso para este test, escribir el nombre y el cuerpo del mismo"
        haceFrioFahrenheit 70 `shouldBe` True
      it "la temperatura pasada a celsius es mayor a 8" $ do
        pendingWith "Escribir un nombre para este test"
        haceFrioFahrenheit 40.4 `shouldBe` True
      it "la temperatura es 8" $ do
        pendingWith "Pensar un caso para este test, escribir el nombre y el cuerpo del mismo"
        haceFrioFahrenheit 46.4 `shouldBe` True
escribiTestsParaEstaFuncion :: SpecWith ()
escribiTestsParaEstaFuncion = pure ()

shouldBeEqualUpTo2Decimals :: Number -> Number -> Expectation
shouldBeEqualUpTo2Decimals aNumber anotherNumber = shouldBeEqualWithErrorLessThan 0.01 aNumber anotherNumber

shouldBeEqualWithErrorLessThan :: Number -> Number -> Number -> Expectation       
shouldBeEqualWithErrorLessThan error aNumber anotherNumber
  | aNumber - anotherNumber < error = pure () -- Esto hace que el test de verde!
  | otherwise = expectationFailure (show aNumber ++ " no es igual (comparando con error < " ++ show error ++ ") a " ++ show anotherNumber)
