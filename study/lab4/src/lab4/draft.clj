(ns lab4.draft)

;с лекции
(defn variable [name]
  {:pre [(keyword? name)]}
  (list :var name))
(defn variable? [expr]
  (= (first expr ) :var))
(defn variable-name [v]
  (second v))
(defn same-variables? [v1 v2]
  (and
    (variable? v1)
    (variable? v2)
    (= (variable-name v1) (variable-name v2))))

(cons :aaa [1 2])
(cons :aaa '(1 2))

(defn sum [expr & rest]
  (cons :sum (cons expr rest)))

(defn sum? [expr]
  (= :sum (first expr)))

(defn args [expr]
  (rest expr))

(defn invert [expr]
  (list :inv expr))
; У нас не сумма а 4 операции

;отрицание
(defn negation [expr & rest]
  (cons :negation (cons expr rest)))

(defn negation? [expr]
  (= :negation (first expr)))

;конъюнкция
(defn conjunction [expr & rest]
  (cons :conjunction (cons expr rest)))

(defn conjunction? [expr]
  (= :conjunction(first expr)))
;дизъюнкция
(defn disjunction [expr & rest]
  (cons :disjunction (cons expr rest)))

(defn disjunction? [expr]
  (= :disjunction (first expr)))
;импликация
(defn implication [expr & rest]
  (cons :negation (cons expr rest)))

(defn implication? [expr]
  (= :negation (first expr)))

(def const-true (list :true))
(def const-false (list :false))


(defn const-true? [expr]
  (= (first expr ) :true))

(defn const-false? [expr]
  (= (first expr ) :false))

(defn const? [expr]
  (or (const-false? expr)
      (const-true? expr)))


(defn diff [expr vr]
  ((some (fn [rule] (if ((first rule) expr vr) (second rule) false))
         diff-rules)
   expr vr))

(defn compute [expr]
  ((some (fn [rule] (if ((first rule) expr) (second rule) false))
         rules)
   expr))
;сделать так чтобы разные правила можно было вкладывать
(defn compute [expr rules]
  ((some (fn [rule] (if ((first rule) expr) (second rule) false))
         rules)
   expr))
(declare fkfk)
(prn)
(pr)
;определяем лист правил вида
(defn diff-rules
  (list
    [pred1 transform1]
    [pred2 transform2]
    ...))

(defn define-rules [var val]
  (list
    [(fn [expr] (and (variable? expr) (same-variables? var expr)))
     (fn [expr] val)]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]
    ))

(defn define-expr [var val expr]
  (compute expr (define-rules var val)))
;сделали замену переменной на константу
;теперь добавим объявление функций коъюнкции,дизъюнкции и отрицания

; можно как на лекции через declare сделать
(declare define-expr)
(defn define-rules [var val]
  (list
    [(fn [expr] (and (variable? expr) (same-variables? var expr)))
     (fn [expr] val)]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (define-expr var val (second expr))))]
    [(fn [expr] (conjunction? expr))
     (apply conjunction (map #(define-expr var val %) (args expr)))]
    [(fn [expr] (disjunction? expr))
     (apply disjunction (map #(define-expr var val %) (args expr)))]
    ))
;работает
; 1 шаг алгоритма заменить все логические операции на конъюнкцию,диъюнкцию и отрицание
(declare step1-expr)
(def step1-rules
  (list
    [(fn [expr] (implication? expr))
     (fn [expr] (step1-expr (disjunction
                              (negation (step1-expr (first (args expr))))
                              (step1-expr (second (args expr))))))]
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (step1-expr  (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(step1-expr %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(step1-expr %) (args expr))))]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]))
(defn step1-expr [expr]
  (compute expr step1-rules))
;работает
;2 шаг алгоритма применить все знаки отрицания чтобы они относились не к формулам а к перменным
(declare step2-expr)
(def step2-rules
  (list
    [(fn [expr] (and (negation? expr) (negation? expr)))
     (fn [expr] (step2-expr (first (second (args expr)))))]
    [(fn [expr] (and (negation? expr) (const-true? (first (args expr)))))
     (fn [expr] (const-false))]
    [(fn [expr] (and (negation? expr) (const-false? (first (args expr)))))
     (fn [expr] (const-true))]
    [(fn [expr] (and (negation? expr) (conjunction? expr)))
     (fn [expr] (step2-expr (apply conjunction (map #(negation %) (args (second expr))))))]
    [(fn [expr] (and (negation? expr) (disjunction? expr)))
     (fn [expr] (step2-expr (apply disjunction (map #(negation %) (args (second expr))))))]

    [(fn [expr] (negation? expr))
     (fn [expr] (negation (step2-expr  (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(step2-expr %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(step2-expr %) (args expr))))]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]))
(defn step2-expr [expr]
  (compute expr step2-rules))
;---------------------------------------------------------------------
;второй шаг алгоритма
(declare step2-expr)
(def step2-rules
  (list
    [(fn [expr] (and (negation? expr) (conjunction? (second expr))))
     (fn [expr] (step2-expr (apply conjunction (map #(negation %) (args (second expr))))))]
    [(fn [expr] (and (negation? expr) (disjunction? (second expr))))
     (fn [expr] (step2-expr (apply disjunction (map #(negation %) (args (second expr))))))]
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (step2-expr  (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(step2-expr %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(step2-expr %) (args expr))))]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]))
(defn step2-expr [expr]
  (compute expr step2-rules))
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;третий шаг алгоритма
(declare step3-expr)
(def step3-rules
  (list
    [(fn [expr] (and (negation? expr) (negation? (second expr))))
     (fn [expr] (step3-expr (first (args (second expr)))))]
    [(fn [expr] (and (negation? expr) (const-true? (first (args expr)))))
     (fn [expr] const-false)]
    [(fn [expr] (and (negation? expr) (const-false? (first (args expr)))))
     (fn [expr] const-true)]
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (step3-expr  (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(step3-expr %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(step3-expr %) (args expr))))]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]))
(defn step3-expr [expr]
  (compute expr step3-rules))
;---------------------------------------------------------------------
;F\/(G/\Q)=(F\/G)/\(F\/Q)
[(fn [expr] (and (disjunction? expr) (conjunction? (second (args expr)))))
 (fn [expr] (step4-expr (conjunction
                          (disjunction
                            (first (args expr))
                            (first (args (second (args expr)))))
                          (disjunction
                            (first (args expr))
                            (second (args (second (args expr))))))))]
;(G/\Q)\/F=(G\/F)/\(Q\/F)
[(fn [expr] (and (disjunction? expr) (conjunction? (second expr))))
 (fn [expr] (step4-expr (conjunction
                          (disjunction
                            (first (args (first (args expr))))
                            (second (args expr)))
                          (disjunction
                            (second (args (first (args expr))))
                            (second (args expr))))))]
;эти функции не нужны потому что у нас  днф
;---------------------------------------------------------------------
;четвертый шаг алгоритма
(declare step4-expr)
(def step4-rules
  (list
    ;F/\(G\/Q)=(F/\G)\/(F/\Q)
    [(fn [expr] (and (conjunction? expr) (disjunction? (second (args expr)))))
     (fn [expr] (step4-expr (disjunction
                              (conjunction
                                (first (args expr))
                                (first (args (second (args expr)))))
                              (conjunction
                                (first (args expr))
                                (second (args (second (args expr)))))))
       )]
    ;(G\/Q)/\F=(G/\F)\/(Q/\F)
    [(fn [expr] (and (conjunction? expr) (disjunction? (second expr))))
     (fn [expr] (step4-expr (disjunction
                              (conjunction
                                (first (args (first (args expr))))
                                (second (args expr)))
                              (conjunction
                                (second (args (first (args expr))))
                                (second (args expr))))))]

    [(fn [expr] (negation? expr))
     (fn [expr] (negation (step4-expr  (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(step4-expr %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(step4-expr %) (args expr))))]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]))
(defn step4-expr [expr]
  (compute expr step4-rules))
;---------------------------------------------------------------------