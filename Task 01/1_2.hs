{-  Задание 1.2

Реализовать на Haskell следующие функции:

    • Синус или косинус числа на основе ряда Тейлора или Маклорена
    • Наибольший общий делитель двух чисел
    • Является ли заданная дата (число, месяц, год) корректным числом с учётом високосных годов 
        и количества дней в месяце?
-}

module Main where

-- Скроем функцию gcd из стандартной библиотеки
import Prelude hiding (gcd)

{--  Наибольший общий делитель двух чисел   --}
-- http://www.cleverstudents.ru/divisibility/nod_finding.html
gcd :: Int -> Int -> Int
gcd a b | a == 0 && b == 0 = error "gcd 0 0 не определен"
        | a < 0 || b < 0 = gcd (abs a) (abs b)  -- Для отрицательных чисел берем их по модулю
        | a == 0 || b == 0 = a + b  -- Одно из чисел равно нулю - возвращаем второе
        | a == b = a
        | a > b = gcd (a - b) b
        | a < b = gcd a (b - a)
{--                                         --}


{--  Синус на основе ряда Тейлора   --}
-- http://newman.livejournal.com/168590.html
tSin :: Double ->   Int ->          Double
--      радианы
--                  кол-во слагаемых
--                  в ряду Тейлора
tSin x 0 = x
tSin x n = tSin x (n-1) + (-1)^n * x^(2*n+1) / fact (2*n+1)
            where fact f = fromIntegral $ product [1..f]
            -- Приводим целочисленное значение произведения всех элементов списка
            -- к значению произвольного числового типа Num (содержит дробный подтип)
{--                                 --}


{-- Проверка корректности даты  --}
-- Тип данных для записи даты
data Date = Date { dd :: Int, mm :: Int, yyyy :: Int }
            deriving(Show,Eq)   -- Haskell автоматически выводит функции печати (Show) 
                                -- и проверки на равенство (Eq) для значений типа

-- Високосный год делится на 4, исключая те года, которые делятся на 100 и
-- не делятся на 400 (1900 - не високосный, 2000 — високосный)
isLeapYear yyyy = yyyy `mod` 4 == 0 && (yyyy `mod` 100 /= 0 || yyyy `mod` 400 == 0)

-- Формула для получения количества дней в месяце, кроме февраля
-- https://habrahabr.ru/post/261773/
dayInMonth mm = 30 + (mm + mm `div` 8) `mod` 2

-- Функция проверки корректности даты, учитываются: попадания чисел в рамки дней/месяцев/лет,
-- февраль, високосность года
isCorrectDate (Date dd mm yyyy) | yyyy >= 0 && mm /= 2 && mm > 0 && mm < 13 && dd > 0 && dd <= dayInMonth mm = True
                                | yyyy >= 0 && mm == 2 && dd > 0 && isLeapYear yyyy == False && dd <= 28 = True
                                | yyyy >= 0 && mm == 2 && dd > 0 && isLeapYear yyyy == True && dd <= 29 = True
                                | otherwise = False

-- Вариативное добавление нуля ко дню или месяцу
zeroAdd n   | n < 10 = "0" ++ show n
            | otherwise = show n

-- Функция для красивого вывода результата проверки
checkDate d@(Date dd mm yyyy)   | isCorrectDate d = zeroAdd dd ++ "." ++ zeroAdd mm ++ "." ++ show yyyy ++ " is correct."
                                | otherwise = zeroAdd dd ++ "." ++ zeroAdd mm ++ "." ++ show yyyy ++ " is incorrect!"
{--                             --}

d1 = Date 02 02 1995
d2 = Date 02 12 1995
d3 = Date 29 02 1996
d4 = Date 30 02 1996
                                
main :: IO ()
main = do
        putStrLn " "
        print $ map checkDate [d1, d2, d3, d4]
        putStrLn " "
        print $ tSin 1.5 12
        putStrLn " "
        print $ gcd 1071 (-462)
        print $ gcd 0 2
        print $ gcd 13 3
        --print $ gcd 0 0
        putStrLn " "