{-  Задание 6.2

Напишите функцию 
    sinPrecisions :: Double -> Stream Double,
которая содержит в себе все значения синусов числа на входе с растущей степенью точности, 
вычисленные с помощью ряда Маклорена для синуса.

-}

module Main where

data Stream a = Cons a (Stream a)

-- Экземпляр класса отображения в строку, встречая NaN заканчивает вывод
instance (Eq a, Show a) => Show (Stream a) where
    show lst = "[" ++ show' lst ++ ", ...]"
        where
            show' (Cons a next) | a /= a = show a
                                | otherwise = show a ++ ", " ++ show' next
--

-- По сути, две одинаковые формулы, работают для (-2π .. 2π)
-- https://www.calc.ru/Ryad-Teylora-Ryady-Maklorena.html
macSin :: Double -> Int -> Double
macSin x 0 = x
macSin x n = dividend / divider + macSin x (n - 1)
    where dividend = (-1)^n * x^(2*n + 1)
          divider = fromIntegral $ product [1..(2*n + 1)]
--

-- http://www.webmath.ru/poleznoe/formules_8_20.php
macSin2 :: Double -> Int -> Double
macSin2 x 1 = x
macSin2 x n = dividend / divider + macSin2 x (n - 1)
    where dividend = (-1)^(n+1) * x^(2*n - 1)
          divider = fromIntegral $ product [1..(2*n - 1)]
--

--
sinPrecisions :: Double -> Stream Double
sinPrecisions x = toinfinity x 0 where
    toinfinity x n = Cons (macSin x n) (toinfinity x (n + 1))
--
sinPrecisions2 :: Double -> Stream Double
sinPrecisions2 x = toinfinity x 1 where
    toinfinity x n = Cons (macSin2 x n) (toinfinity x (n + 1))
--

main :: IO ()
main = do
        print . show $ sinPrecisions 2.1
        print . show $ sinPrecisions2 2.1