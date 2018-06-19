{-  Задание 1.1
Дана следующая структура данных:

data Term = IntConstant{ intValue :: Int }    
            | Variable{ varName :: String }    
            | BinaryTerm{ lhv :: Term, rhv :: Term } deriving(Show,Eq)

Данная структура данных представляет собой ячейку дерева разбора некоторого языка, 
содержащего в себе числовые константы, переменные и бинарные операторы.

Следует расширить данную структуру данных таким образом, чтобы она позволяла описывать 
различные бинарные операторы (сложение, умножение, вычитание), а так же унарный минус. 
Помимо этого, нужно описать ряд функций:
    • Операторы <+>, <-> и <*>, которые создают значения соответствующих бинарных операций.
    • Функцию замены replaceVar, которая принимает имя переменной и терм, на который её 
      следует заменить, и производит замену этой переменной на этот терм по всему выражению.
-}
module Main where

-- Из библиотеки Prelude исключим подключение функции (<*>), чтобы не было конфликтов
import Prelude hiding((<*>))

-- Создадим свой тип для типов операций, содержащий нульарные конструкторы
data Operation  = Add 
                | Sub 
                | Mul
                | Neg
                deriving(Show,Eq)

-- Расширим тип конструктором для унарных выражений и
-- добавим к полям конструкторов унарных и бинарных выражений тип операции
data Term   = IntConstant   { intValue :: Int }    
            | Variable      { varName :: String }    
            | UnaryTerm     { op :: Operation, value :: Term }
            | BinaryTerm    { op :: Operation, lhv :: Term, rhv :: Term } 
            deriving(Show,Eq)   -- Haskell автоматически выводит функции печати (Show) 
                                -- и проверки на равенство (Eq) для значений типа

-- Объявляем функции для инфиксного использования
(<+>) l r = BinaryTerm Add l r
(<->) l r = BinaryTerm Sub l r
(<*>) l r = BinaryTerm Mul l r

-- Префиксное использование, приоритет 10
(-) v   = UnaryTerm Neg v

-- Для инфиксных функций приоритет задаём явно
infixl 5 <+>, <->
infixl 6 <*>

replaceVar :: String ->     Term ->     Term ->     Term
--            эту переменную
--                          в этом терме
--                                      заменяем на
--                                      этот терм

-- Для терма константного значения возвращаем его же
replaceVar _ (IntConstant intValue) _ = IntConstant intValue

-- Если имена переменных сходятся, возвращаем новый терм;
-- иначе - не заменяем переменную и возвращаем ее терм
replaceVar var v@(Variable varName) term    | varName == var = term
                                            | otherwise = v

-- Для унарных/бинарных выражений рекурсивно вызываем функцию для их операндов-термов
replaceVar var (UnaryTerm op value) term    = UnaryTerm op $ replaceVar var value term
replaceVar var (BinaryTerm op lhv rhv) term = BinaryTerm op t1 t2
                                                where
                                                    t1 = replaceVar var lhv term 
                                                    t2 = replaceVar var rhv term

my01 = IntConstant 20
my02 = IntConstant 5
my03 = Variable "varX"

main :: IO ()
main = do
        let t1 = my01 <-> my02 <*> my03
            t2 = UnaryTerm Sub $ Variable "varY"
        putStrLn " "
        print $ replaceVar "varX" t1 t2
        putStrLn " "