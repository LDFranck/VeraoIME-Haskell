-- Atividade Avaliativa Parte D:
-- Provar que os tipos '3*a' e 'a+a+a' sao isomorfos.

module Codigo where
    
ida :: (Maybe Bool, a) -> Either (Either a a) a
ida (Just False, a) = Left (Left a)
ida (Just True, a) = Left (Right a)
ida (Nothing, a) = Right a

volta :: Either (Either a a) a -> (Maybe Bool, a)
volta (Left (Left a)) = (Just False, a)
volta (Left (Right a)) = (Just True, a)
volta (Right a) = (Nothing, a)
