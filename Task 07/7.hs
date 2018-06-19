{-  Задание 7

Описанная в лекции структура данных «очередь» представляет собой одностороннюю очередь. 
В то же самое время достаточно часто необходимо использовать двустороннюю очередь 
(или «деку»). Покажите, как с помощью модификаций односторонней очереди получить 
аналогичную структуру данных, но поддерживающую операции двусторонней очереди. 
Операции должны иметь наилучшие показатели сложности, которых вы (как вы считаете) 
можете достичь.

Реализуйте предложенную структуру.

Оцените амортизированную сложность операций над предложенной структурой данных 
методами банкира и физика.

-}

module Main where

{-     В лекции односторонняя очередь имела стек входящих и стек исходящих элементов,
    для двусторонней очереди эти стеки превращаются просто в левый и правый.
    С ними обоими можно проводить одни и те же операции - заталкивать (push) и 
    выталкивать (pop) элементы.
       Также при опустении исходящего стека мы перебрасывали в него весь входящий
    стек, если он был не пуст. Для двусторонней очереди логичным будет перебрасывать
    половину другого стека в пустой: в обоих стеках будет одинаковое кол-во элементов
    (ну, за исключением тех случаев, когда общее кол-во элементов будет нечетным).
-}
    
--         левый стек v
data Deque a = Deque [a] [a]
--            правый стек ^

-- Для наглядного отображения инвертируем стек исходящих, чтоб как на картинке было
instance (Show a) => Show (Deque a) where
    show (Deque [] []) = "[]><[]"
    show (Deque _in out) = show _in ++ "><" ++ show (reverse out)
--

-- Создание пустой очереди
emptyDeque :: Deque a
emptyDeque = Deque [] []
--

-- Делим список на два и возвращаем их в паре
takeHalves :: [a] -> ([a], [a])
takeHalves list  = splitAt (length list `div` 2) list
--

-- Заталкиваем в левый стек
pushLeft :: Deque a -> a -> Deque a
pushLeft (Deque _in out) x = Deque (x:_in) out
--

-- Заталкиваем в правый стек
pushRight :: Deque a -> a -> Deque a
pushRight (Deque _in out) x = Deque _in (x:out)
--

-- Выталкиваем из левого стека
popLeft :: Deque a -> (Deque a, a)
popLeft (Deque [] []) = error "Deque is empty"
popLeft (Deque (h:t) out) = (Deque t out, h)
popLeft (Deque [] out) = popLeft $ Deque (reverse $ snd half) (fst half)
    where half = takeHalves out
--

-- Выталкиваем из правого стека
popRight :: Deque a -> (Deque a, a)
popRight (Deque [] []) = error "Deque is empty"
popRight (Deque _in (h:t)) = (Deque _in t, h)
popRight (Deque _in []) = popRight $ Deque (fst half) (reverse $ snd half)
    where half = takeHalves _in
--

--
my02 = pushRight (pushRight (pushLeft (pushLeft emptyDeque 1) 3) 2) 6
(my03, a) = popRight my02
(my04, b) = popRight my03
(my05, c) = popRight my04
(my06, d) = popLeft my02

main :: IO ()
main = do
        print . show $ my02
        putStrLn " "
        print . show $ (my03, a)
        print . show $ (my04, b)
        print . show $ (my05, c)
        print . show $ (d, my06)

{-
Метод банкира:
    • За каждое добавление и обычное удаление элемента получаем $1.
    • Если при удалении возникает потребность перебросить из одного стека в другой N/2 элементов,
      это будет стоить $N/2, при этом $N мы уже точно заработали.
    Получается, что данный набор операций имеет амортизированную сложность O(1).

Метод физика:
    • Потенциал - это кол-во элементов в очереди (сумма размеров двух стеков). 
    • Так как минимальный размер стека - 0, потенциал никогда не опускается ниже нуля.
    • Добавление в любой из двух стеков увеличивает потенциал на 1.
    • Обычное удаление не изменяет потенциал.
    • Удаление с переброской стека уменьшает потенциал на N и занимает O(N).
    • Перебрасываемое кол-во элементов не может быть больше размера очереди и между
      перебрасываниями не может быть менее N/2 операций, значит потенциал не опускается ниже нуля.
    Получается, что данный набор операций имеет амортизированную сложность O(1).
-}