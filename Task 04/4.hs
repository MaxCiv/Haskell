{-  Задание 4

Интересным (хоть и совершенно бесполезным) примером монады является обычная функция () -> a.

Рассмотрим следующий тип данных:

    data FunMonad a = FunMonad{ fun :: () -> a }

Реализуйте для этого типа данных класс Monad. Покажите (на примерах) его работу.
-}

module Main where

data FunMonad a = FunMonad { fun :: () -> a }

-- Монада должна являться функтором
instance Functor FunMonad where
    fmap f (FunMonad a) = FunMonad $ \() -> f (a ()) 
--

-- Для монады обязательно реализуются функции return (упаковка в монаду),
-- bind (связывания) и fail (ошибки)
instance Monad FunMonad where
    return a  = FunMonad $ \() -> a
    a >>= f =  f $ fun a ()     -- bind
    fail = error
--

-- Понятное отображение монады
instance (Show a) => Show (FunMonad a) where
    show (FunMonad a) = "FunMonad (" ++ (show $ a ()) ++ ")"
--

-- Также для монады нужно реализовать экземпляр класса типов Applicative
instance Applicative FunMonad where
    pure a = FunMonad $ \() -> a
    FunMonad a <*> FunMonad b = FunMonad $ \() -> (a ()) (b ())
--

m01 =  FunMonad $ \()-> 96   
f01 x = FunMonad $ \() -> 2 ^ x
demo01 = fmap (`mod` 28) m01 -- FunMonad (12)
demo02 = m01 >>= f01         -- FunMonad (79228162514264337593543950336)

       
main :: IO ()
main = do
        putStrLn " "
        print . show $ demo01
        print . show $ demo02