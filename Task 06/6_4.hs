{-  Задание 6.4

На основе типа данных BinaryTree из предыдущих заданий и идей из двусвязного списка
реализуйте бинарное дерево поиска, в котором каждая нода содержит ссылку на родителя.
Реализуйте основные операции.

-}

module Main where

-- По сути, в дереве нет пустых узлов, поэтому EmptyTree не имеют поля parent
data BinaryTree = EmptyTree
                | Node {value::Integer, l::BinaryTree, r::BinaryTree, parent::BinaryTree}
                deriving (Eq, Show)
--

-- Добавление элемента в дерево
insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree x = Node x EmptyTree EmptyTree EmptyTree -- добавление в пустое дерево
insert (Node v EmptyTree r p) x | x < v = newParent -- вставляем влево родителя, который содержит новый узел,
                           where  newParent = Node v newNode r p -- который ссылается на нового родителя
                                  newNode = Node x EmptyTree EmptyTree newParent
insert (Node v l EmptyTree p) x | x > v = newParent -- вставляем вправо родителя, который содержит новый узел, 
                           where  newParent = Node v l newNode p
                                  newNode = Node x EmptyTree EmptyTree newParent
insert n@(Node v l r p) x | x < v = Node v (insert l x) r p -- спускаемся влево
                          | x > v = Node v l (insert r x) p -- спускаемся вправо
                          | otherwise = n -- в дереве уже присутствует такое значение
--

-- Удаление элемента из дерева
remove :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree _ = EmptyTree -- не нашли элемент, ничего не удалили
remove (Node v l r p) x | x < v = Node v (remove l x) r p -- спускаемся влево
                        | x > v = Node v l (remove r x) p -- спускаемся вправо
                        | otherwise = join l r -- удаляем узел, заменяя его 
                                               -- соединением потомков
      where join n EmptyTree = n
            join n (Node v l r p) = Node v (join l n) r p
--

-- Поиск элемента в дереве (проверка его существования)
containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False -- дерево закончилось и элемент не найден
containsElement (Node v l r _) x | v == x = True  -- элемент найден
                                 | v >  x = containsElement l x   -- спуск по дереву
                                 | v <  x = containsElement r x   --
--
my01 = insert (insert (insert (insert EmptyTree 8) 3) 11) 1

--

main :: IO ()
main = do
        print . show $ containsElement my01 5
        print . show $ containsElement (insert my01 5) 5
        print . show $ containsElement (remove my01 3) 3
        print . show $ containsElement (remove my01 8) 3
        print . show $ my01 -- зацикливается, значит все работает