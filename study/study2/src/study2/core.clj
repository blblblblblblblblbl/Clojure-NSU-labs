(ns study2.core (:require [clojure.string :as s]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(+ 1 1)  ;alt+shift+p to send command in repl
(+ 2 1)
(+ 1 3)
(println "Hello, World!")
(+ (* 23 3) 15)
(- 34 22 (- 10))

(def lang "clojure")       ;объявление костанты
(println lang)

(def lang (atom "clojure"))                                 ;объявление переменной но лучше переменные по-другому делать
(reset! lang "aaaaa")                                       ;изменение

(def a (atom 0))
(swap! a inc)                                               ;увеличение на 1
(swap! a dec)                                               ;уменьшение на 1

(def a "a")
(defonce a "b")                                             ;не переопределяет если такое уже было
(println a)                                                 ;выведет a

(def a "a")
(def a "b")                                             ;переопределяет если такое уже было
(println a)                                                 ;выведет b

(fn [n] (* n n n))                                          ;объявление анонимной функции но просто вызвать ее нельзя, надо чере def объявить c именем
(def cube (fn [n] (* n n n)))                               ;так
(defn cube [n] (* n n n))                                 ;или так
(println (cube 4))
((fn [n] (* n n n)) 2)                                      ;либо сразу при объявлении можно вызвать аноним функцию

(let [text "lorem"] (println text)) ; => lorem

; форма записи с локальными объявлениями:
(let [sum (fn [x y] (+ x y))]
  (sum 8 7)) ;

;Реализуйте функцию prod-sum, которая сначала умножает переданное число на себя,
; а затем суммируется между собой и полученным результатом умножения.
; Воспользуйтесь локальными объявлениями для хранения промежуточных результатов вычисления.
(defn prod-sum [x]
  (let [prod (* x x)]
    (+ prod x)))

;Реализуйте функцию leap-year?, которая проверяет, является ли год високосным.
(defn leap-year? [a] (and (= (rem a 4) 0) (or (not= (rem a 100) 0) (and (= (rem a 400) 0) (= (rem a 100) 0)))))
;или
(defn leap-year? [a]
  (letfn [(divisible? [x y] (zero? (rem x y)))]
    (and (divisible? a 4) (or (not (divisible? a 100)) (and (divisible? a 400) (divisible? a 100))))))

;Реализуйте функцию sentence-type, которая возвращает строку "cry",
; если переданый текст написан заглавными буквами, и возвращает строку "common" в остальных случаях.
(defn sentence-type [x]
  (if (= x (upper-case x)) "cry" "common"))

(defn say-boom [x] (when (= x "go") (println "Boom!")))
(say-boom "go")
(say-boom "hey")

(case v                                                     ;Вместо switch
  0 "zero"
  1 "one"
  2 "two")

(cond                                                       ;там, где обычно применяется if-else_if
  (pos? -5) "first return"
  (zero? -5) "second return"
  (pos? 5) "third return"
  :else "boom!")

(list 1 2 3)                                                ;список

;Реализуйте функцию maps, которая должна принимать два списка
;— список функций и список списков аргументов —
;и возвращать список результатов применения функций к наборам аргументов
(defn maps [a b]
  (map map a b))


;Реализуйте функцию increment-numbers, которая берёт из списка-аргумента значения,
; являющиеся числами (number?) и возвращает список этих чисел,
; увеличив предварительно каждое число на единицу (inc)
(defn increment-numbers [x]
  (map inc (filter number? x)))

(max 1 2 3)

(defn max-delta [a b]
  (max (map Math/abs (map - a b))))
(Math/abs 42)
(Math/abs (list 4 5 2))
 (reduce max  (map - (list 1 2 3) (list 4 5 3)))
(map inc  (list 1 2))
(defn abs [a] (Math/abs a))
(map abs (list -1 2))

(defn max-delta [a b]
  (reduce max (map (fn [x] (Math/abs x)) (map - a b))))
(max-delta
  (list 10 -15 35)
  (list 2 -12 42))
(max (list 10 -15 35))

(defn max-delta [a b]
  (reduce max (map (fn [x] (Math/abs x)) (map - a b))))
(max-delta '(10 -15 35) '(2 -12 42))

(map list (list 1 2 3) (list 1 2 3))

;Реализуйте функцию max-delta, которая должна принимать два списка чисел
; и вычислять максимальную разницу
; (абсолютное значение разницы) между соответствующими парами элементов.
(defn max-delta [xs ys]
  (reduce (fn [acc [x y]] (max acc (Math/abs (- x y))))
          0 (map list xs ys)))



(def my-list '(1 2 3))
(first my-list) ; => 1
(last my-list)  ; => 3
(rest my-list)  ; => '(2 3)


;Реализуйте функцию lookup, которая бы должна принимать аргумент-ключ
; и список пар "ключ-значение" и возвращать либо пару "ключ-значение",
; где ключ равен первому аргументу, либо возвращать false, если подходящих пар
; в списке не нашлось. Если подходящих пар найдётся несколько, вернуть нужно первую.
(defn lookup [a b]
  (filter = a b))
(defn ff [x y] (if(= x y) true false))
(filter ff  (list 1 (list 1 2 3)))

(def user-ages
  (list (list "Tom" 31)
        (list "Alice" 22)
        (list "Bob" 42)))

(defn lookup [key pairs]
  (letfn [(same-key? [kv] (= key (first kv)))]
    (let [found-pairs (filter same-key? pairs)]
      (if (empty? found-pairs)
        false
        (first found-pairs)))))
(lookup "Bob" user-ages)

;сумма списка через рекурсию
(defn sum [vals]
  (if (empty? vals) 0 (let [head (first vals) tail (rest vals)] (+ head (sum tail)))))
 (sum (list 1 2 3))
;сумма через хвостовой вызов
(defn sumh
  ([vals] (sumh vals 0))                                    ;аноним функция
  ([vals acc]                                               ;аноним функция
   (if (empty? vals)
     acc
     (recur (rest vals) (+ (first vals) acc)))))
(sumh (list 1 2 3))

;Реализуйте функцию skip, которая должна принимать два аргумента —
;целое число n и список — и возвращать новый список,
;содержащий все элементы из первого списка за исключением
; n первых элементов. Если n окажется больше, чем количество
; элементов во входном списке, результатом должен быть пустой список.
(defn skip [n spisok]
(if (<= n 0) spisok
             (if (empty? spisok) '()
                                 (let [tail (rest spisok)] (skip (- n 1) tail)))))
(skip 4 (list 1 2 3))

 (= 1 (first '(1 2 3)))
(def my-str "hello")
(def list-str (s/split my-str #""))                         ;разделение на список
(println list-str)
(defn revers [lst]
  (if (= (first lst) lst) lst
                   (let [tail (rest lst)] (list rest (revers tail)))))
 (println (revers list-str))