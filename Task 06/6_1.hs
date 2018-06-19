{-  Задание 6.1

Напишите функции вставки, удаления и индексации для типа двунаправленных списков DList:

    data DList a = DEmpty | DCons (DList a) a (DList a)

-}

module Main where

data DList a = DEmpty
             | DCons (DList a) a (DList a)
--

-- Перевод списка в двунаправленный список
list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DEmpty
list2dlist' left (h:t) = rec where
    rec = DCons left h (list2dlist' rec t)

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DEmpty lst
--

-- Экземпляр класса равенства
instance (Eq a) => Eq (DList a) where
    DEmpty == DEmpty = True
    (DCons _ lh lt) == (DCons _ rh rt) = (lh == rh) && (lt == rt)
--

-- Экземпляр класса отображения в строку
instance (Show a) => Show (DList a) where
    show lst = "[" ++ show' lst ++ "]"
        where
            show' DEmpty = ""
            show' (DCons _ h DEmpty) = show h
            show' (DCons _ h t) = show h ++ ", " ++ show' t
--

-- Вставка элемента (считаем с нуля, как настоящие программисты)
insertAt :: DList a -> a ->     Int -> DList a
--          в список
--                     вставляем
--                     элемент
--                              на эту
--                              позицию
insertAt _ _ n | n < 0 = error "Index out of range"
insertAt DEmpty elem 0 = DCons DEmpty elem DEmpty
insertAt DEmpty _ _ = error "Index out of range"
insertAt (DCons b val t) elem 0 = insertAt' -- нашли позицию и вставляем элемент
    where
        insertAt' = DCons b elem (DCons insertAt' val t)
        -- левый хвост оставляем, вставляем новый элемент и старый
        -- сдвигаем вправо по тому же принципу
insertAt (DCons b val t) elem n = DCons b val $ insertAt t elem (n - 1) -- ищем позицию
--

-- Удаление элемента по значению
delete :: Eq a => DList a -> a -> DList a
delete (DCons b val t@(DCons _ val2 t2)) elem | val == elem = DCons b val2 t2 -- перезаписываем элемент следующим за ним
                                              | otherwise = DCons b val $ delete t elem -- продолжаем поиск
delete dlst@(DCons _ val DEmpty) elem | elem == val = DEmpty   -- удаляем последний элемент
                                      | otherwise = dlst    -- элемент не был найден, ничего не удаляем
--

-- Удаление элемента по позиции
deleteAt :: Eq a => DList a -> Int -> DList a
deleteAt _ n | n < 0 = error "Index out of range"
deleteAt (DCons b val t@(DCons _ val2 t2)) n | n == 0 = DCons b val2 t2 -- удаляем элемент
                                             | otherwise = DCons b val $ deleteAt t (n - 1) -- ищем позицию
deleteAt dlst@(DCons _ _ DEmpty) n | n == 0 = DEmpty -- удаляем последний элемент
                                   | otherwise = dlst    -- элемент не был найден, ничего не удаляем
--

-- Получение элемента по индексу
getAt :: DList a -> Int -> a
getAt _ n | n < 0 = error "Index out of range"
getAt DEmpty _ = error "DList is empty"
getAt (DCons _ val _) 0 = val
getAt (DCons _ val t) n = getAt t (n - 1)
--
    
    
my01 = list2dlist [0,11,22,33,44,55,66]

main :: IO ()
main = do
        print . show $ my01
        print . show $ getAt my01 2
        print . show $ insertAt my01 28 4
        print . show $ delete my01 66
        print . show $ delete my01 44
        print . show $ delete my01 32
        print . show $ deleteAt my01 6
        print . show $ deleteAt my01 2
        print . show $ deleteAt my01 9