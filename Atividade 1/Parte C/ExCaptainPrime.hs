-- Atividade Avaliativa Parte C:
-- Escolher 2 exercicios nivel MEDIUM do HackerRank e resolve-los em Haskell
-- Exercicio 2: Captain Prime

module ExCaptainPrime where

ehPrimo :: Integer -> Bool
ehPrimo n = length [x | x <- [1..n], mod n x == 0] == 2

temZero :: Integer -> Bool
temZero n = '0' `elem` (show n)

pelaEsquerda :: String -> Bool
pelaEsquerda [x] = ehPrimo (read [x] :: Integer)
pelaEsquerda xs = ehPrimo (read (tail xs) :: Integer) && pelaEsquerda (tail xs)

pelaDireita :: String -> Bool
pelaDireita [x] = ehPrimo (read [x] :: Integer)
pelaDireita xs = ehPrimo (read (init xs) :: Integer) && pelaDireita (init xs)

data PosicaoNavio = DEAD | LEFT | RIGHT | CENTRAL deriving(Show)
definePosicao :: Integer -> PosicaoNavio
definePosicao idNum
    | temZero idNum == True = DEAD
    | ehPrimo idNum == False = DEAD
    | otherwise = case vePosicao of
        3 -> CENTRAL
        2 -> RIGHT
        1 -> LEFT
        0 -> DEAD
        where vePosicao = (1*)(fromEnum $ pelaEsquerda (show idNum)) + (2*)(fromEnum $ pelaDireita (show idNum))

main :: IO ()
main = do
    numInputs <- readLn
    let loop cont = do
        if (cont/=0) then do
            varId <- readLn
            print (definePosicao varId)
            loop (cont-1)
        else do return ()
    loop numInputs
