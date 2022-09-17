(ns lab1.core)

(add-symbol (list  "d") (list "a"))
(if (empty? `(())) true false) ; false
(if (empty? ()) true false) ;true
(cons (first (list "a")) (list "d"))  ;=> ("a" "d")
(concat (list "a") (list ()))
(cons (cons (first (list "a")) (list "d")) ())  ;=> (("a" "d"))
(cons (cons (first (list "a")) (list "d")) (cons (cons (first (list "a")) (list "d")) ())) ;=> (("a" "d") ("a" "d"))
(add-alphabet-to-word (list  "d") (list "a"))       ;=> (("a" "d") ("b" "d") ("c" "d"))

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
(defn add-symbol [combination symbols]             ; эта функция дописывает букву в начало слова и возвращает список слов например если подали (list  "d") (list "a") ;=> (("a" "d") ("b" "d") ("c" "d"))
  (if (empty? symbols)                                      ;condition
    ()                                                      ;true
    (if (not= (first combination) (first symbols))        ;false and new cond
      (cons (cons (first symbols) combination) (add-symbol combination (rest symbols)))      ;true
      (add-symbol combination (rest symbols))                                       ;false
      )
    )
  )

(defn next-gen-comb [combinations symbols]  ;использует функцию add-symbol просто для всех слов а не для одного и выдает список новых комбинаций
  (if (empty? combinations)                          ;condition
    ()               ;true
    (concat (add-symbol (first combinations) symbols) (next-gen-comb (rest combinations) symbols))   ;false
    ))

(defn my-loop [combinations symbols n]         ;данная функция нужна нам вместо цикла чтобы проделать шаг n раз и довести длину комбинация до n
  (if (= n 1)
    (next-gen-comb combinations symbols)
    (my-loop (next-gen-comb combinations symbols) symbols (dec n))))


(defn my-loop [combinations symbols n]         ;данная функция нужна нам вместо цикла чтобы проделать шаг n раз и довести длину комбинация до n
  (if (= n 0)
    combinations
    (my-loop (next-gen-comb combinations symbols) symbols (dec n))))
;///////////////////////////////////////////////////////////////сделать чтобы для n=0 и без новых условий

(defn func [symbols n]                                      ;требуемая к реализации функция
  (if (not= n 0)                                            ;condition
    (my-loop (list(list)) symbols n)                                   ;true
    ()                                                      ;false
    ))







(func (list "a" "b" "c" "d") 3)
(func (list [1 "sfg"] "c" :f) 3)


