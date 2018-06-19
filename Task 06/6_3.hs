{-  Задание 6.3

Напишите функцию 
    ePrecisions :: Stream Rational,
которая содержит в себе приближённые значения числа e
в виде рациональных чисел с растущей точностью.
Используйте любой подходящий способ вычисления числа e.

-}

module Main where

data Stream a = Cons a (Stream a) deriving (Show) -- не всё так просто с Show у Rational

-- Для расчета экспоненты используются те же ряды Маклорена
-- https://www.calc.ru/Ryad-Teylora-Ryady-Maklorena.html
macE :: Rational -> Int -> Rational
macE x 0 = 1
macE x n = dividend / divider + macE x (n - 1)
    where dividend = x ^ n
          divider = fromIntegral $ product [1..n]
--

--
ePrecisions :: Stream Rational
ePrecisions = toinfinity 1 0 where
    toinfinity x n = Cons (macE x n) (toinfinity x (n + 1))
--


main :: IO ()
main = do
        print . show $ ePrecisions