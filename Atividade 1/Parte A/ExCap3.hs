-- Atividade Avaliativa Parte A:
-- Exercicios do livro Haskell: Uma introducao a programacao funcional
-- 2 exercicios do Capitulo 3 que envolvam criacao de tipos de dados.

module ExCap3 where

-- 3.1) Crie o tipo Pergunta com os values constructors Sim e Nao. Faca as
-- funcoes seguintes, determinando seus tipos explicitamente: <...>
data Pergunta = Sim | Nao deriving(Show)

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

listPergs :: [Pergunta] -> [Int]
listPergs x = map pergNum x

and' :: Pergunta -> Pergunta -> Pergunta
and' x y
    | foo == 0 = Nao
    | otherwise = Sim
    where foo = pergNum x * pergNum y

or' :: Pergunta -> Pergunta -> Pergunta
or' x y =
    let foo = pergNum x + pergNum y
    in case foo of
            0 -> Nao
            otherwise -> Sim

not' :: Pergunta -> Pergunta
not' Sim = Nao
not' _ = Sim

-- 3.2) Faca o tipo Temperatura que pode ter valores Celsius, Fahrenheit ou
-- Kelvin. Implemente as funcoes de conversao entre as escalas de temperatura.
data Temperatura = Celsius | Fahrenheit | Kelvin deriving(Show)

converterCelsius :: Double -> Temperatura -> Double
converterCelsius x Kelvin = x - 273.15
converterCelsius x Fahrenheit = (x - 32) * 5/9
converterCelsius x _ = x

converterKelvin :: Double -> Temperatura -> Double
converterKelvin x tipo = case tipo of
    Celsius -> x + 273.15
    Fahrenheit -> (x - 32) * 5/9 + 273.15
    otherwise -> x

converterFahrenheit :: Double -> Temperatura -> Double
converterFahrenheit x tipo = foo tipo x
    where foo Celsius = (\var -> var * 9/5 + 32)
          foo Kelvin = (\var -> converterFahrenheit (converterCelsius var Kelvin) Celsius)
          foo Fahrenheit = (\var -> var)
-- 3 tipos diferentes de solucao para um problema semelhante
