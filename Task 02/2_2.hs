{-  Задание 2.2

1. Реализуйте функции foldl и foldr из лекции.
2. На основе функций foldl и foldr реализуйте свои версии функций:
    • map :: (a -> b) -> [a] -> [b]  
    • flatMap :: (a -> [b]) -> [a] -> [b]  
    • concat :: [a] -> [a] -> [a]  
    • filter :: (a -> Boolean) -> [a] -> [a]  
    • maxBy :: (a -> Integer) -> [a] -> a  
    • minBy :: (a -> Integer) -> [a] -> a  
    • reverse :: [a] -> [a]  
    • elementAt :: Integer -> [a] -> a  
    • indexOf :: String -> [String] -> Integer   
-}

module Main where

-- Исключаем функции, которые будем переопределять
import Prelude hiding (foldl, foldr, map, concat, filter, reverse)

-- Левоассоциативная свёртка: будучи примененной к своим параметрам - функции
-- с двумя аргументами, начальному значению (левому аргументу функции)
-- и списку, - сокращает список, используя функцию слева направо.
-- Расчет происходит как бы с начала списка.
-- Типы a и b могут быть одинаковыми.
foldl :: (b -> a -> b) ->               b ->        [a] ->  b
--       функция с агрументами типов
--       b и a, возвращает тип b
--                                      начальное
--                                      значение
--                                                  список
foldl _ z [] = z    -- для конца списка/пустого списка возвращаем начальное значение
foldl f z (x:xs) = foldl f (f z x) xs   -- результат применения функции f к начальному значению и голове
                                        -- списка становится начальным значением для следующего вызова функции f, 
                                        -- вторым аргументом которой является оставшийся список - хвост
--

-- Правоассоциативная свёртка: будучи примененной к своим параметрам - функции
-- с двумя аргументами, начальному значению (левому аргументу функции)
-- и списку, - сокращает список, используя функцию справа налево.
-- Расчет происходит как бы с конца списка.
-- Из-за режима ленивых вычислений правоассоциативная свёртка может остановиться на пол-пути,
-- если функция f не требует значения её второго аргумента при некотором значении первого.
foldr :: (a -> b -> b) ->               b ->        [a] -> b
--       функция с агрументами типов
--       a и b, возвращает тип b
--                                      начальное
--                                      значение
--                                                  список
foldr _ z [] = z    -- для конца списка/пустого списка возвращаем начальное значение
foldr f z (x:xs) = f x (foldr f z xs)
--

-- Отображение - применение функции к каждому элементу списка и возврат списка с результатами.
-- Map не только применяет унарную функцию ко всем элементам списка, но и
-- формирует список результатов с помощью функции (:). Используется правая свертка
-- вида f x1 (f x2 (... (f xn-1 xn))) для совмещенной функции (:) . f
-- Используем бесточечную нотацию, чтобы не писать последний аргумент - список.
map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []
--

-- Такое же отображение, только в результате объединяется вместе получившийся набор списков.
-- Список формируется с помощью функции (++) - склейка второго списка в конец первого.
flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f = foldr ((++) . f) []
--

-- Конкатенация двух списков в один, соответствует функции (++)
-- Нужно добавлять к началу ys по одному элементу xs, начиная с конца.
-- По одному элементу xs - сворачивается xs. Начиная с конца - свертка правая.
-- Добавлять к началу ys - ys будет исходным значением, использоваться будет функция (:).
concat :: [a] -> [a] -> [a]
concat xs ys =  foldr (:) ys xs
--

-- Из списка вычисляется список, содержащий элементы, которые удовлетворяют предикату. 
filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr cond []    -- бесточечная нотация (следствие каррирования) позволяет не писать последний аргумент
--                    ^  ^
           where cond x xs | f x       = x:xs  -- составляем список из тех элементов, что проходят проверку
                           | otherwise = xs    -- остальные - отбрасываем
--

-- Поиск наибольшего элемента среди списка, который обрабатывается функцией
maxBy :: (a -> Int) -> [a] -> a
maxBy _ [] = error "maxBy can't take Empty List"
maxBy f (x:xs) = foldr fun x xs
                 where fun x xs | (f x) > (f xs) = x -- если прошлый элемент больше, оставляем его
                                | otherwise = xs -- иначе - возвращаем хвост для дальнейшего поиска
--

-- Поиск наименьшего элемента среди списка, который обрабатывается функцией
minBy :: (a -> Int) -> [a] -> a
minBy _ [] = error "minBy can't take Empty List"
minBy f (x:xs) = foldr fun x xs
                 where fun x xs | (f x) < (f xs) = x -- если прошлый элемент больше, отбрасываем его
                                | otherwise = xs -- иначе - возвращаем хвост для дальнейшего поиска
--

-- Возвращает элементы списка в обратном порядке
-- Можно выполнить правую свёртку функцией (++), где мы будем присоединять в конец голову списка (++[x]).
-- Однако функция конкатенации требует больше ресурсов, чем добавление в начало (:), поэтому reverse можно переписать.
reverseR :: [a] -> [a]
reverseR = foldr (\ x y -> y ++ [x]) [] -- в этот раз используем локальную лямбда-функцию
--

-- Возвращает элементы списка в обратном порядке
-- Левая свёртка - проходим список от начала к концу. Снимаем голову у исходного списка и добавляем ее в 
-- начало нового списка (изначально пустого). Для больших списков этот вариант будет быстрее.
reverse :: [a] -> [a]
reverse = foldl (flip (:)) []   -- flip - меняет порядок аргументов у функции (:)
--

-- Возвращает значение i-ого элемента в списке, начало нумерации с единицы
-- Функция zip формируем список пар вида (индекс,значение). Лямбда-функция сверяет запрашиваемый индекс и 
-- индекс проверяемой пары и, если они равны, возвращает эту пару. Иначе, возвращает прошлую пару.
-- В конце из полученной пары возвращается второй элемент - значение. Поиск осуществляется с начала списка. 
elementAt :: Int -> [a] -> a
elementAt i l@(x:xs) | i < 1 = error "Invalid index"
                     | i > length l = error "Index too large"
                     | length l == 0 = error "Empty List"
                     | otherwise = snd $ foldl (\p (index,y) -> if index == i then (index,y) else p) (1,x) (zip [2,3..] xs)
--

-- Возвращает индекс запрашиваемого элемента в списке.
-- Принцип работы схож с функцией elementAt за исключением того, что сравниваются искомое значение
-- и значение пары, а возвращается первый элемент пары - индекс.
indexOf :: String -> [String] -> Integer
indexOf s l@(x:xs)  | length l == 0 = error "Empty List"
                    | otherwise = fst $ foldl (\p (index,y) -> if y == s then (index,y) else p) (1,x) (zip [2,3..] xs)
-- 


main :: IO ()
main = do
        putStrLn " "
        print $ map (/2) [2, 4, 6, 8, 10, 13]
        putStrLn " "
        print $ flatMap id [[],[1,2],[3],[],[4,5,6]]
        putStrLn " "
        print $ concat [[1,2],[3],[4,5,6]] [[7,8],[9],[4,5,6]]
        putStrLn " "
        print $ filter (>5) [1,2,3,4,5,6,7,8]
        putStrLn " "
        print $ maxBy abs [1,2,5,6,-7,3,4]
        putStrLn " "
        print $ minBy abs [2,-1,5,6,-7,3,4]
        putStrLn " "
        print $ reverse [1..5]
        putStrLn " "
        print $ elementAt 5 [1..5]
        putStrLn " "
        print $ indexOf "34" ["12", "23", "34", "35"]
        putStrLn " "