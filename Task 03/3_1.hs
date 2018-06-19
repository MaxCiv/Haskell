{-  Задание 3.1

Дана следующая структура данных:

data WeirdPeanoNumber = Zero 
                      | Succ (WeirdPeanoNumber ) 
                      | Pred (WeirdPeanoNumber )
    где Zero это 0, Succ X это (X + 1), Pred X это (X - 1).
	
Реализуйте для данного представления чисел все характерные для целых чисел классы типов. 
Проверьте работу реализованных функций.
-}

module Main where

data WeirdPeanoNumber = Zero 
                      | Succ (WeirdPeanoNumber) 
                      | Pred (WeirdPeanoNumber)
--

-- Перевод из странного числа в целое
weirdToInt :: WeirdPeanoNumber -> Integer
weirdToInt Zero = 0
weirdToInt (Succ a) = weirdToInt a + 1
weirdToInt (Pred a) = weirdToInt a - 1
--

-- Перевод из целого числа в странное
intToWeird :: Integer -> WeirdPeanoNumber
intToWeird a | a > 0 = Succ $ intToWeird (a - 1)
             | a < 0 = Pred $ intToWeird (a + 1)
             | otherwise = Zero
--

---- Экземпляры классов типа WeirdPeanoNumber 
---- (реализуем только минимально требуемые определения)
-- Экземпляр класса равенства
instance Eq WeirdPeanoNumber where
    (==) Zero Zero = True
    (==) (Succ a) (Succ b) = a == b
    (==) (Pred a) (Pred b) = a == b
    (==) _ _ = False
--

-- Экземпляр класса упорядочивания
instance Ord WeirdPeanoNumber where
    (<=) Zero (Pred _) = False
    (<=) Zero _ = True
    (<=) (Succ a) (Succ b) = a <= b
    (<=) (Succ a) _ = False
    (<=) (Pred a) (Pred b) = a <= b
    (<=) (Pred a) _ = True
--

-- Экземпляр класса отображения в строку
instance Show WeirdPeanoNumber where
	show = show . weirdToInt
--

-- Экземпляр класса арифметических операций
instance Num WeirdPeanoNumber where
    (+) Zero b = b
    (+) a Zero = a
    (+) (Succ a) (Succ b) = Succ $ (Succ a) + b
    (+) (Succ a) (Pred b) = a + b
    (+) (Pred a) (Pred b) = Pred $ (Pred a) + b
    (+) (Pred a) (Succ b) = a + b
    
    (*) Zero _ = Zero
    (*) _ Zero = Zero
    (*) (Pred a) (Pred b) = Succ $ a * b - a - b
    (*) (Pred a) (Succ b) = Pred $ a * b + a - b
    (*) (Succ a) (Pred b) = Pred $ a * b - a + b
    (*) (Succ a) (Succ b) = Succ $ a * b + a + b
    
    abs (Pred a) = Succ (abs a)
    abs a = a

    signum Zero = Zero
    signum (Succ _) = Succ Zero
    signum (Pred _) = Pred Zero

    fromInteger a | (a == 0) = Zero
                  | (a > 0)  = Succ $ fromInteger (a - 1)
                  | (a < 0)  = Pred $ fromInteger (a + 1)
                  
    negate Zero = Zero
    negate (Pred a) = Succ $ negate a
    negate (Succ a) = Pred $ negate a
--

-- Экземпляр класса действительных чисел
instance Real WeirdPeanoNumber where
    toRational Zero = toRational 0
    toRational (Succ a) = toRational a + 1
    toRational (Pred a) = toRational a - 1
--

-- Экземпляр класса перечисления
instance Enum WeirdPeanoNumber where
    toEnum a | (a == 0) = Zero
             | (a > 0)  = Succ $ toEnum (a - 1)
             | (a < 0)  = Pred $ toEnum (a + 1)

    fromEnum Zero = 0
    fromEnum (Succ a) = fromEnum a + 1
    fromEnum (Pred a) = fromEnum a - 1
--

-- Экземпляр класса целых чисел
instance Integral WeirdPeanoNumber where
    toInteger Zero = 0
    toInteger (Pred a) = toInteger a - 1
    toInteger (Succ a) = toInteger a + 1
    
    quotRem Zero _ = (Zero, Zero)
    quotRem _ Zero = error "Divide by zero"
    quotRem a b | signum a == signum b = absCalc -- если оба Succ или Pred
                | otherwise = (negate $ fst absCalc, (signum a) * (snd absCalc))
                -- если (Succ и Pred) или (Pred и Succ)
                -- отрицаем результат целочисленного деления и выбираем соответствующий знак для остатка
                    
                    -- от остатка начинаем отнимать делитель (d) и считать, сколько раз отняли
                    -- когда остаток становится меньше делителя, возвращаем пару результата
                    where cycleSub p@(quot, rem) d | rem >= d  = cycleSub (quot + 1, rem - d) d
                                                   | otherwise = p
                          -- для расчетов используем абсолютные значения
                          absCalc = cycleSub (Zero, abs a) (abs b)
--
aa = -8
bb = 28
a = intToWeird aa
b = intToWeird bb
--
main :: IO ()
main = do
        print $ show a
        print $ show b
        print . show $ a == b
        print . show $ negate $ b `mod` abs a   -- (-4)
        print . show $ quotRem a b
        print . show $ quotRem b a
        print . show $ quotRem aa bb
        print . show $ quotRem bb aa