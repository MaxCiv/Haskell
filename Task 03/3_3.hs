{-  Задание 3.3

Дана следующая структура данных:

newtype PSet a = PSet{ contains :: (a -> Bool) }
    представляющая собой предикатную реализацию математического понятия «множество».

Реализуйте столько вариантов реализаций классов Monoid и Functor для данной структуры данных, сколько сможете.
Объясните свои решения.
-}

module Main where

newtype PSet1 a = PSet1{ contains1 :: (a -> Bool) }
newtype PSet2 a = PSet2{ contains2 :: (a -> Bool) }
newtype PSet3 a = PSet3{ contains3 :: (a -> Bool) }

-- Пересечение множеств (A && B)
instance Monoid (PSet1 a) where
    mempty = PSet1 (\a -> True)
    mappend (PSet1 a) (PSet1 b) = PSet1 (\x -> (a x) && (b x))
--

-- Объединение множеств (A || B)
instance Monoid (PSet2 a) where
    mempty = PSet2 (\a -> False)
    mappend (PSet2 a) (PSet2 b) = PSet2 (\x -> (a x) || (b x))
--

-- Разность множеств (A && not B) реализовать нельзя, ведь не выполняется
-- основной закон моноида: x `mappend` mzero === mzero `mappend` x === x

-- Симметрическая разность множеств (A && not B) || (not A && B)
instance Monoid (PSet3 a) where
    mempty = PSet3 (\a -> False)
    mappend (PSet3 a) (PSet3 b) = PSet3 (\x -> ((a x) && (not $ b x)) || ((not $ a x) && (b x)))
--

-- fmap :: (a -> b) -> f a -> f b
-- Если у нас есть только отображение из множества A в B, то мы не можем
-- получить никакой информации о множестве В и нормально реализовать работу
-- функтора, поэтому он просто возвращает False.
instance Functor PSet1 where
    fmap _ _ = PSet1 (\_ -> False)
--