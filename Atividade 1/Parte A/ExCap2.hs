-- Atividade Avaliativa Parte A:
-- Exercicios do livro Haskell: Uma introducao a programacao funcional
-- 2 exercicios do Capitulo 2 utilizando funcoes map, filter e unfold.

module ExCap2 where

-- 2.4) Escreva uma funcao que receba um vetor de Strings e retorne uma lista
-- com o tamanho de cada String. As palavras de tamanho par devem ser excluidas
-- da resposta.
solucaoDoisQuatro :: [String] -> [Int]
solucaoDoisQuatro vetor = filter (\x -> x `mod` 2 /= 0) (map length vetor)

-- 2.1) e) Gerar a lista [1.0, 0.5, 0.25, 0.125, 0.03125]
solucaoDoisUmE :: [Float]
solucaoDoisUmE = unfold (\x -> if x<0.03125 then Nothing else Just(x, x/2)) 1

-- Funcao auxiliar unfold:
unfold :: (b -> Maybe (a,b)) -> b -> [a]
unfold func val =
    case func val of
        Nothing -> []
        Just (x,y) -> x : unfold func y
