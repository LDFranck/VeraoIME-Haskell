-- Resolucao dos exercicios propostos no capitulo 2 do livro:
-- Haskell: Uma introducao a programacao funcional - Alexandre Garcia
module ExerciciosCap2 where

-- 2.1)
-- LEMBRAR: lista = [EXP(x,y,...) | x <- [...], y <- [...], ..., FILTROS]
-- a) [11^x | x <- [0..6]]
-- b) [x | x <- [1..40], x `mod` 4 /= 0]
-- c) ['A':x:"BB" | x <- ['a'..'g']]
-- d) [5+3*x | x <- [0..12], not $ elem (5+3*x) [14,23,35]]
-- e) [1/x | x <- [2^y | y <- [0..5]]]
-- f) [1+9*x | x <- [0..7]]
-- g) [2*x | x <- [1..15], not $ x `elem` [3,7,10,13]]
-- h) '@': [x | x <- ['A'..'L'], not $ elem x "BFHIK"]

-- 2.2)
isStringPar :: String -> Bool
isStringPar x = length x `mod` 2 == 0

-- 2.3)
revVetorStr :: [String] -> [String]
revVetorStr vStr = [reverse x | x <- vStr]

-- 2.4)
tamVetorStr :: [String] -> [Int]
tamVetorStr vStr = [length x | x <- vStr, length x `mod` 2 /= 0]

-- 2.5) head = last $ reverse [<lista>]

-- 2.6)
isPalindromo :: String -> Bool
isPalindromo x = x == reverse x

-- 2.7_
multiplosN :: Int -> (Int, Int, Int, Int)
multiplosN x = (2*x, 3*x, 4*x, 5*x)
