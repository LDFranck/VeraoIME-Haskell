-- Atividade Avaliativa Parte B:
-- Escolher 3 exercicios do Advent of Code e resolve-los em Haskell
-- Exercicio 4: Day 3 - Toboggan Trajectory - Parte 2

module ExDay3_2 where

ehArvore :: Char -> Int
ehArvore '#' = 1
ehArvore _ = 0

percorreTrajeto :: (Int, Int) -> [String] -> Int
percorreTrajeto (x,y) mapa = contaArvores 1
    where contaArvores cont
            | cont == length mapa = 0
            | otherwise = ehArvore(last $ take (x*cont+1) $ cycle $ head $ drop cont mapa) + contaArvores (cont+y)

main :: IO ()
main = do
    listaFile <- fmap lines (readFile "inputDay3.txt")
    print (product [percorreTrajeto (x) listaFile | x <- [(1,1),(3,1),(5,1),(7,1),(1,2)]])
