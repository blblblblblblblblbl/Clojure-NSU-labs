(ns lab1.lab13)
;1.3. Определить функции my-map и my-filter, аналогичные map (для одного списка) и filter, выразив
;их через reduce и базовые операции над списками (cons, first, concat и т.п.)

(reduce f nil '(1 2 3))
(letfn [(func [x] (println x))])

;идея реализации my map берем функцию которую подают только меняем ее на функцию которая
;включает в себя эту функцию и объединение в список
(defn my-map [f coll]
   (letfn [(func [acc x] (concat acc (list (f x))))]
     (reduce func nil coll)))

(my-map inc '(1 2 3 4 5 6))  ;=> (2 3 4 5 6 7)
(map inc '(1 2 3 4 5 6))

(my-map (fn [x] (+ 3 x)) '(1 2 3))
(map (fn [x] (+ 3 x)) '(1 2 3))
;///////////////////////////////////////самому reduce написать и вместо concat использовать conjoin

; идея та же только еще включаем с условием
(defn my-filter [f coll]
  (letfn [(func [acc x] (concat acc (if (f x) (list x))))]
    (reduce func nil coll)))

(defn my-reduce [f acc coll]
  (if (empty? coll)
    acc
    (my-reduce f (first coll) (rest coll)) ))
;/////////////////////////////////////////////////////conj к вектору в конец добавляет
(empty? (rest (list 1)))
(+ nil 1)

(my-reduce + 0 (list 1))

(my-filter even? (list 1 2 3 4 5 6 ))
(filter even? (list 1 2 3 4 5 6 ))

(my-filter int? (list 1 2.5 "foo" 7))
(filter int? (list 1 2.5 "foo" 7))

(my-filter pos? (list -1 5 42 0 -100 3))
(filter pos? (list -1 5 42 0 -100 3))
;