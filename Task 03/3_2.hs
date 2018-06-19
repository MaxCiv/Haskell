{-  Задание 3.2

Дана следующая структура данных:

data ReverseList a = RNil | RCons (ReverseList a) a
    представляющая собой перевёрнутый односвязный список элементов типа a. 
    
Реализуйте функции перевода данного представления в обычные списки, 
а также экземпляры классов Eq, Ord, Show, Monoid и Functor. 
Конструкцию deriving использовать, разумеется, нельзя.
-}

module Main where

data ReverseList a = RNil 
                   | RCons (ReverseList a) a
--

-- Перевод в обычный список toList
fromRevList :: ReverseList a -> [a]
fromRevList RNil = []
fromRevList (RCons t h) = h:(fromRevList t)
--

-- Перевод в перевёрнутый список
toRevList :: [a] -> ReverseList a
toRevList [] = RNil
toRevList (h:t) = RCons (toRevList t) h
--

-- Экземпляр класса равенства
instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) RNil _ = False
    (==) _ RNil = False
    (==) (RCons a b) (RCons aa bb) = a == aa && b == bb
--

-- Экземпляр класса упорядочивания
instance (Ord a) => Ord (ReverseList a) where
    (<=) RNil _ = True
    (<=) _ RNil = False
    (<=) (RCons a b) (RCons aa bb) = b <= bb || a <= aa
--

-- Экземпляр класса отображения в строку
instance (Show a) => Show (ReverseList a) where
    show = show . fromRevList
--

-- Экземпляр класса моноид
instance Monoid (ReverseList a) where
    mempty = RNil
    mappend x RNil = x
    mappend RNil y = y
    mappend x (RCons y v) = RCons (mappend x y) v
--

-- Экземпляр класса функтор
instance Functor ReverseList where
    fmap _ RNil = RNil
    fmap f (RCons xs x) = RCons (fmap f xs) (f x)
--

my01 = toRevList [8, 3, 11, 1, 5, 9, 14, 6, 10, 12, 15, 7, 13]
my02 = RNil

main :: IO ()
main = do
        print . show $ my01 == my01
        print . show $ my01 == my02
        print . show $ my01 == (fmap (*2) my01)
        print . show $ my01 `mappend` my01
        print . show $ fmap (*2) my01