-- Atividade Avaliativa Parte D:
-- Provar que os tipos '3*a' e 'a+a+a' sao isomorfos.

module Codigo where
    
data ParteD a = Opcao1 a | Opcao2 a | Opcao3 a deriving Show

ida :: (Maybe Bool, a) -> ParteD a
ida (Just False, x) = Opcao1 x
ida (Just True, x) = Opcao2 x
ida (Nothing, x) = Opcao3 x

volta :: ParteD a -> (Maybe Bool, a)
volta (Opcao1 x) = (Just False, x)
volta (Opcao2 x) = (Just True, x)
volta (Opcao3 x) = (Nothing, x)
