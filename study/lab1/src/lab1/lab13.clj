(ns lab1.lab13)
;1.3. Определить функции my-map и my-filter, аналогичные map (для одного списка) и filter, выразив
;их через reduce и базовые операции над списками (cons, first, concat и т.п.)

(reduce f nil '(1 2 3))
(letfn [(func [x] (println x))])

;идея реализации my map берем функцию которую подают только меняем ее на функцию которая
;включает в себя эту функцию и объединение в список
(defn my-map [f coll]
   (letfn [(func [acc x] (concat acc (list (f x))))]
     (my-reduce func nil coll)))

(defn my-map [f coll]   ;через conj
  (letfn [(func [acc x] (conj acc (f x)))]
    (reverse (my-reduce func nil coll))))
(defn my-map [f coll]   ;через conj
  (letfn [(func [acc x] (conj acc (f x)))]
    (reverse (my-reduce func nil coll))))
; вместо reverse [] как работает concat без reverse и на списках виесто векторов
(my-map inc '(1 2 3 4 5 6))  ;=> (2 3 4 5 6 7)
(map inc '(1 2 3 4 5 6))

(my-map (fn [x] (+ 3 x)) '(1 2 3))
(map (fn [x] (+ 3 x)) '(1 2 3))
;///////////////////////////////////////самому reduce написать и вместо concat использовать conjoin
;/////////////////////////////////////////////////////conj к вектору в конец добавляет
; идея та же только еще включаем с условием
(concat (list 1 2) (list 3 4))                              ; 1 2 3 4
(conj (list 1 2) 3)                                         ; 3 1 2
(conj [1 2] 3)                                              ; 1 2 3

(defn my-filter [f coll]
  (letfn [(func [acc x] (concat acc (if (f x) (list x))))]
    (my-reduce func nil coll)))

(defn my-filter [f coll]                                    ;через conj
  (letfn [(func [acc x] (if (f x) (conj acc x) acc))]
    (reverse (my-reduce func nil coll))))

(my-filter even? (list 1 2 3 4 5 6))

(defn my-reduce [f acc coll]                                ;свой reduce
  (if (empty? (rest coll))
    (f acc (first coll))
    (my-reduce f (f acc (first coll)) (rest coll))
    ))
; тут нет recur значит будет stack over flow для большой коллекции
; что с my-reduce и concat


(conj '(1 2 3))
(conj [1 2 3] 4)
(conj '(1 2 3) 4)
(cons (list 5 6) (list 2 3 4))
(concat '(1 2) '(3 4))
(concat '(1 2) '(3) )
;/////////////////////////проверка
(my-reduce + 0 ())

(concat () (list 1 2))
(take 10 (concat () (list 1 2)))
(nth (concat () (list 1 2)) 3)
(cons (first (list 1 2)) (concat (rest (list 1 2)) (list 3 4)))

(chunk-cons (chunk (chunk-buffer 32)) (seq (range 42)))

()
(lazy-seq
  (let [s (seq x)]
    (if s
      (cons (first s) (concat (rest s) y))
      y)))

(chunk-buffer 32)
(chunk (chunk-buffer 32))
(seq (range 42))

(chunk-rest
  (chunk-cons (chunk (chunk-buffer 32))
              (seq (range 42))))


(empty? (rest (list 1)))
(+ nil 1)
(reduce + (list 1 2 3))
(my-reduce + 0 (list 1 2 3))

(my-reduce + 0 '())
(+ 1 nil)

(my-filter even? (list 1 2 3 4 5 6 ))
(filter even? (list 1 2 3 4 5 6 ))

(my-filter int? (list 1 2.5 "foo" 7))
(filter int? (list 1 2.5 "foo" 7))

(my-filter pos? (list -1 5 42 0 -100 3))
(filter pos? (list -1 5 42 0 -100 3))
;
;///////////////////////////////////////////////////////////////////////////////////////
;просто записи, читать не надо
;///////////////////////////////////////////////////////////////////////////////////////
(defn my-reduce [f acc coll]
  (if (empty? coll)
    acc
    (f acc (my-reduce f (first coll) (rest coll)))
    ))

(defn my-reduce [f acc coll]
  (if (empty? coll)
    acc
    (f (my-reduce f (first coll) (rest coll)) acc)
    ))

(defn my-reduce [f acc coll]
  (if (empty? (rest coll))
    (f acc (first coll))
    (f acc (my-reduce f (first coll) (rest coll)))
    ))

(defn my-reduce [func acc coll]
  (if (empty? coll)
    acc
    (recur func (func acc (first coll)) (rest coll))))