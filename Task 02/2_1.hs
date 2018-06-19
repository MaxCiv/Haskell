{-  Задание 2.1

Реализуйте структуру данных "бинарное дерево поиска" для целых чисел без балансировки. 
Реализация включает функции:

    • Добавления элемента: insert :: BinaryTree -> Integer -> BinaryTree
    • Удаления элемента: remove :: BinaryTree -> Integer -> BinaryTree
    • Создания пустого дерева: emptyTree :: BinaryTree
    • Поиска элемента в дереве: containsElement :: BinaryTree -> Integer -> Bool
    • Поиска в дереве наименьшего элемента, который больше или равен заданному: 
        nearestGE :: BinaryTree -> Integer -> Integer
    • Создания дерева из списка: treeFromList :: [Integer] -> BinaryTree
    • Создания списка из дерева: listFromTree :: BinaryTree -> [Integer]
    • Операторы insert и remove должны поддерживать цепочки вызовов в инфиксной форме: 
        listFromTree (emptyTree `insert` 1 `insert` 2 `insert` 3) === [1,2,3]
-}

module Main where

-- Бинарное дерево поиска - значение узла больше значения левого подузла 
-- и меньше значения правого
data BinaryTree = EmptyTree     -- пустое дерево/узел
                | Node { value :: Integer, l :: BinaryTree, r :: BinaryTree }
                -- узел, содержащий значение и свои левый и правый подузлы
                deriving (Eq, Show)
--

-- Добавление элемента в дерево
insert :: BinaryTree -> Integer -> BinaryTree
--        в это дерево
--                      добавляем
--                      этот элемент
insert EmptyTree x = Node x EmptyTree EmptyTree -- добавление в пустой узел
insert n@(Node v l r) x | x < v = Node v (insert l x) r   -- спускаемся влево
                        | x > v = Node v l (insert r x)   -- спускаемся вправо
                        | otherwise = n -- в дереве уже присутствует такое значение
--

-- Удаление элемента из дерева
remove :: BinaryTree ->     Integer -> BinaryTree
--        из этого дерева
--                          удаляем
--                          этот элемент
remove EmptyTree y = error "Can't remove from Empty Tree"
remove (Node v l r) x | x < v  = Node v (remove l x) r   -- поиск элемента по дереву
                      | x > v  = Node v l (remove r x)
                      | x == v = keepStructure l r  -- удаление без потери лишних узлов 
                      where                         -- может пойти по трем сценариям
                        keepStructure EmptyTree EmptyTree = EmptyTree   -- у узела нет подузлов, удаляем его
                        keepStructure EmptyTree r = r   -- заменяем узел его единственным правым подузлом
                        keepStructure l EmptyTree = l   -- заменяем узел его единственным левым подузлом
                        keepStructure l r = Node xnew l rnew    -- Возвращается перестроенная ветка
                            where 
                                xnew = findLowerLeft r  -- поиск в правом подузле значения его самого нижнего левого подузла
                                rnew = remove r xnew    -- удаление этого значения
                                
                                -- О перестроенной ветке
                                -- В последствии, на месте удаленного узла появляется узел с 
                                -- самым левым значением из правого подузла удаленного узла.
                                -- Новым же правым подузлом становится дерево без того значения, 
                                -- которое теперь стоит в узле.
                                
                                -- Поиск значения самого нижнего левого подузла
                                findLowerLeft (Node lower EmptyTree _) = lower
                                findLowerLeft (Node _ l _)   = findLowerLeft l
--

-- Создание пустого дерева
emptyTree :: BinaryTree
emptyTree = EmptyTree
--

-- Поиск элемента в дереве (проверка его существования)
containsElement :: BinaryTree -> Integer ->         Bool
--                 в этом дереве
--                               ищем это значение
containsElement EmptyTree _ = False -- дерево закончилось и элемент не найден
containsElement (Node v l r) x | v == x = True  -- элемент найден
                               | v >  x = containsElement l x   -- спуск по дереву
                               | v <  x = containsElement r x   --
--

-- Вспомогательная функция для поиска наименьшего элемента, большего или равного заданному, 
-- по левой подветке, которая помнит последнее подходящее значение элемента
scanLeft :: BinaryTree -> Integer -> Integer-> Integer
scanLeft EmptyTree x mem = mem    -- дойдя до конца ветки, возвращаем запомненное значение
scanLeft (Node v l r) x mem | x == v = x -- найден элемент, равный заданному
                           | x > v = scanLeft r x mem   -- заданный элемент больше проверяемого, спускаемся в правую подветку
                           | x < v = scanLeft l x v     -- заданный элемент меньше проверяемого, спускаемся в левую подветку
                                                        -- запоминая при этом новое значение текущего проверяемого элемента

-- Поиск в дереве наименьшего элемента, который больше или равен заданному
nearestGE :: BinaryTree -> Integer -> Integer
nearestGE EmptyTree x = error "Element not found or this is Empty Tree"
nearestGE (Node v l r) x | x == v = x   -- найден элемент, равный заданному
                         | x > v  = nearestGE r x   -- заданный элемент больше проверяемого, спускаемся в правую подветку
                         | x < v  = scanLeft l x v  -- заданный элемент меньше проверяемого, спускаемся в левую подветку,
                                                    -- запоминая при этом значение текущего проверяемого элемента
--

-- Создание дерева из списка
treeFromList :: [Integer] -> BinaryTree
treeFromList [] = EmptyTree -- для пустого списка возвращаем пустое дерево/узел
treeFromList [h] = insert EmptyTree h   -- добавление последнего элемента списка
treeFromList (h:t) = Node h (treeFromList less) (treeFromList greater)  -- добавление головы списка в узел и рек. вызов для хвоста
                    where   less = filter (<h) t    -- список с оставшимися элементами, которые меньше значения h
                            greater = filter (>h) t -- список с оставшимися элементами, которые больше значения h
--

-- Создание списка из дерева
listFromTree :: BinaryTree -> [Integer]
listFromTree EmptyTree = [] -- для пустого дерева возвращаем пустой список
listFromTree (Node v EmptyTree EmptyTree) = v:[]    -- для конечного узла возвращаем значение узла и конец списка
listFromTree (Node v l r) = [v] ++ listFromTree l ++ listFromTree r -- склеиваем списки без глобальной сортировки по значению
                                                                    -- (listFromTree l ++ [v] ++ listFromTree r),
                                                                    -- таким образом, получим список из которого
                                                                    -- можно будет воссоздать дерево
--


--myTree01 = emptyTree `insert` 8 `insert` 3 `insert` 11 `insert` 1 `insert` 5 `insert` 9 `insert` 14
--        `insert` 6 `insert` 10 `insert` 12 `insert` 15 `insert` 7 `insert` 13

myTree01 = treeFromList [8, 3, 11, 1, 5, 9, 14, 6, 10, 12, 15, 7, 13]

myTree02 = myTree01 `remove` 11

main :: IO ()
main = do
        putStrLn " "
        print $ show myTree01
        putStrLn " "
        print $ show myTree02
        putStrLn " "
        print . show $ nearestGE myTree02 11
        putStrLn " "
        print . show $ listFromTree myTree02
        putStrLn " "
        print . show $ treeFromList (listFromTree myTree02)
        putStrLn " "