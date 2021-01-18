-- Atividade Avaliativa Parte B:
-- Escolher 3 exercicios do Advent of Code e resolve-los em Haskell
-- Exercicio 2: Day 2 - Password Philosophy - Parte 2

module ExDay2_2 where

quebraInput :: String -> [String]
quebraInput x = words $ filter (':'/=) $ map (\var -> if var == '-' then ' ' else var) x

separaInput :: [String] -> (Int, Int, Char, String)
separaInput (x:y:z:w:[]) = (read x, read y, head z, w)

verificaRegra :: (Int, Int, Char, String) -> Bool
verificaRegra (x,y,z,w) = ((last $ take x w) == z) /= ((last $ take y w) == z)

somaLoop :: Int -> [String] -> Int
somaLoop 0 _ = 0
somaLoop x y = (fromEnum $ verificaRegra $ formataInput $ head y) + somaLoop (x-1) (drop 1 y)
    where formataInput = (separaInput . quebraInput)

main :: IO ()
main = do
    listaFile <- fmap lines (readFile "inputDay2.txt")
    print (somaLoop (length listaFile) listaFile)
