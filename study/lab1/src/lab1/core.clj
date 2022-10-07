(ns lab1.core)


;Задан набор символов и число n. Опишите функцию, которая возвращает список всех строк длины n,
;состоящих из этих символов и не содержащих двух одинаковых символов, идущих подряд.
;Для символов 'а', 'b', 'c' и n=2 результат должен быть
;("ab" "ac" "ba" "bc" "ca" "cb") с точностью до перестановки.

;lab 1.1 Решите задачу с помощью элементарных операций над последовательностями и рекурсии

;идея решение: будем составлять комбинации так
;сначала у нас нету ничего
;потом берем и составляем комбинацию из одного символа
;далее приписываем по одному символу за шаг попутно проверяя чтобы символ не повторялся и запоминая новые комбинации
;проделываем данный шаг пока длина наших комбинаций не будет равна n
(defn add-symbol [combination symbols]
  (if (empty? symbols)
    ()
    (if (not= (first combination) (first symbols))
      (cons (cons (first symbols) combination) (add-symbol combination (rest symbols)))
      (add-symbol combination (rest symbols))
      )
    )
  )

(defn next-gen-comb [combinations symbols]
  (if (empty? combinations)
    ()
    (concat (add-symbol (first combinations) symbols) (next-gen-comb (rest combinations) symbols))
    ))


(defn my-loop [combinations symbols n]
  (if (= n 0)
    combinations
    (my-loop (next-gen-comb combinations symbols) symbols (dec n))))

(defn func [symbols n]
  (if (not= n 0)
    (my-loop (list(list)) symbols n)
    ()
    ))



