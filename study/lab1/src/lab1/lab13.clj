(ns lab1.lab13)
;1.3. Определить функции my-map и my-filter, аналогичные map (для одного списка) и filter, выразив
;их через reduce и базовые операции над списками (cons, first, concat и т.п.)

(defn my-reduce [func acc coll]
  (if (empty? coll)
    acc
    (recur func (func acc (first coll)) (rest coll))))

(defn my-map [f coll]
  (letfn [(func [acc x] (conj acc (f x)))]
    (reverse (my-reduce func nil coll))))

(my-map inc '(1 2 3 4 5 6))
(map inc '(1 2 3 4 5 6))

(my-map (fn [x] (+ 3 x)) '(1 2 3))
(map (fn [x] (+ 3 x)) '(1 2 3))

(defn my-filter [f coll]
  (letfn [(func [acc x] (if (f x) (conj acc x) acc))]
    (reverse (my-reduce func nil coll))))

(my-filter even? (list 1 2 3 4 5 6))

