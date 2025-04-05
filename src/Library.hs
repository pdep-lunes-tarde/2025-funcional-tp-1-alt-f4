module Library where
import PdePreludat

-- 1. Numeros

siguiente :: Number -> Number
siguiente num = num + 1

esPositivo :: Number -> Bool
esPositivo num = num > 0

inversa :: Number -> Number
inversa num = 1/num

-- 1.5 Bonus OPCIONAL 

perimetroCuadrado :: Number -> Number
perimetroCuadrado lado = lado * 4

superficieCuadrado :: Number -> Number
superficieCuadrado lado = lado * lado

superficieCubo :: Number -> Number
superficieCubo lado = lado * lado * 6

superficieCilindro :: Number -> Number -> Number
superficieCilindro r h = 2 * pi * r * (r + h)

-- 2. Temperaturas

celsiusAFahrenheit :: Number->Number
celsiusAFahrenheit tempC = (tempC * 1.8) + 32

fahrenheitACelsius :: Number->Number
fahrenheitACelsius tempF = (tempF-32)/1.8

-- escriban el tipo de esta función
haceFrioCelsius :: Number -> Bool
haceFrioCelsius tempC = tempC <= 8

-- escriban el tipo de esta función
haceFrioFahrenheit :: Number -> Bool
haceFrioFahrenheit tempF = fahrenheitACelsius tempF <= 8


