-- Atividade Avaliativa Parte C:
-- Escolher 2 exercicios nivel MEDIUM do HackerRank e resolve-los em Haskell
-- Exercicio 1: Super Digit

module ExSuperDigit where

somaDigitos :: Integer -> Integer
somaDigitos x
    | x < 10 = x
    | otherwise = x `mod` 10 + somaDigitos (x `div` 10)

superDigito :: Integer -> Integer
superDigito x
    | x <10 = x
    | otherwise = superDigito $ somaDigitos x

main :: IO ()
main = do
    varIn <- getLine
    let varK = last (map read (words varIn) :: [Integer])
        varN = sum $ map (\x -> read (x:[]) :: Integer) (head (words varIn)) -- "otimizacao" para numeros grandes
    print (superDigito $ (varK*) $ somaDigitos varN)
    -- notar que 148148148 = 1+4+8+1+4+8+1+4+8 = 3*(1+4+8)
