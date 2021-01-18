-- Atividade Avaliativa Parte B:
-- Escolher 3 exercicios do Advent of Code e resolve-los em Haskell
-- Exercicio 1: Day 2 - Password Philosophy - Parte 1

module ExDay2_1 where

quebraInput :: String -> [String]
quebraInput x = words $ filter (':'/=) $ map (\var -> if var == '-' then ' ' else var) x

separaInput :: [String] -> (Int, Int, Char, String)
separaInput (x:y:z:w:[]) = (read x, read y, head z, w)

verificaRegra :: (Int, Int, Char, String) -> Bool
verificaRegra (x,y,z,w)
    | foo >= x && foo <= y = True
    | otherwise = False
    where foo = length (filter (z==) w)

somaLoop :: Int -> [String] -> Int
somaLoop 0 _ = 0
somaLoop x y = (fromEnum $ verificaRegra $ formataInput $ head y) + somaLoop (x-1) (drop 1 y)
    where formataInput = (separaInput . quebraInput)


main :: IO ()
main = do
    listaFile <- fmap lines (readFile "inputDay2.txt")
    print (somaLoop (length listaFile) listaFile)
