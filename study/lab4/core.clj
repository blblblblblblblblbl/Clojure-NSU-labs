(ns lab4.core)

;делаем как на лекции с переменной и суммой только добавляем
;аналогично проверку на true, false и 3 логических операции
(defn variable [name]
  {:pre [(keyword? name)]}
  (list :var name))

(def const-true (list :true))
(def const-false (list :false))

(defn const-true? [expr]
  (= (first expr ) :true))

(defn const-false? [expr]
  (= (first expr ) :false))


(defn variable? [expr]
  (= (first expr ) :var))


(defn const? [expr]
  (or (const-false? expr)
      (const-true? expr)))


(defn variable-name [v]
  (second v))

(defn same-variables? [v1 v2]
  (and
    (variable? v1)
    (variable? v2)
    (= (variable-name v1) (variable-name v2))))

(defn args [expr]
  (rest expr))

(defn invert [expr]
  (list :inv expr))

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
  (cons :implication (cons expr rest)))

(defn implication? [expr]
  (= :implication(first expr)))

(defn compute [expr rules]
  ((some (fn [rule] (if ((first rule) expr) (second rule) false))
         rules)
   expr))

(declare define-expr)
;с помощью этой функции обходим наше выражение и заменяем переменную на ее знначение
(defn define-rules [var val]
  (list
    [(fn [expr] (and (variable? expr) (same-variables? var expr)))
     (fn [expr] val)]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (define-expr var val (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(define-expr var val %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(define-expr var val %) (args expr))))]
    ))

(defn define-expr [var val expr]
  (compute expr (define-rules var val)))

;--------------------------------------------------------------
; 1 шаг алгоритма заменить все логические операции на конъюнкцию,диъюнкцию и отрицание
;первый шаг алгоритма
(declare step1-expr)
(def step1-rules
  (list
    ;A->B=!A\/B
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
;---------------------------------------------------------------------
;2 шаг алгоритма применить все знаки отрицания чтобы они относились не к формулам а к перменным
;второй шаг алгоритма
(declare step2-expr)
(def step2-rules
  (list
    ;!(A/\B)=!A\/!B
    [(fn [expr] (and (negation? expr) (conjunction? (second expr))))
     (fn [expr] (step2-expr (apply disjunction (map #(negation %) (args (second expr))))))]
    ;!(A\/B)=!A/\!B
    [(fn [expr] (and (negation? expr) (disjunction? (second expr))))
     (fn [expr] (step2-expr (apply conjunction (map #(negation %) (args (second expr))))))]


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
;3 шаг убрать двойные отрицания и заменить !true !false
;третий шаг алгоритма
(declare step3-expr)
(def step3-rules
  (list
    ;!!A
    [(fn [expr] (and (negation? expr) (negation? (second expr))))
     (fn [expr] (step3-expr (first (args (second expr)))))]
    ;!true
    [(fn [expr] (and (negation? expr) (const-true? (first (args expr)))))
     (fn [expr] const-false)]
    ;!false
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
;---------------------------------------------------------------------
;свойства дистрибутивности
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
;после подстановки появляются такие штуки (:disjunction (:true) (:negation (:var :y)))
;нужно избавиться от констант
;---------------------------------------------------------------------
;избавляемся от выражения с константами или от костант
;пятый шаг алгоритма
(declare step5-expr)
(def step5-rules
  (list
    [(fn [expr] (and (conjunction? expr) (some const? (args expr))))
     (fn [expr] (if (some const-false? (args expr))
                  const-false
                  (apply conjunction (map #(step5-expr %) (filter #(not (= const-true %)) (args expr))))))]
    [(fn [expr] (and (disjunction? expr) (some const? (args expr))))
     (fn [expr] (if (some const-true? (args expr))
                  const-true
                  (apply disjunction (map #(step5-expr %) (filter #(not (= const-false %)) (args expr))))))]

    [(fn [expr] (negation? expr))
     (fn [expr] (negation (step5-expr  (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(step5-expr %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(step5-expr %) (args expr))))]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]))
(defn step5-expr [expr]
  (compute expr step5-rules))
;---------------------------------------------------------------------
; теперь такие штуки появляются (:conjunction (:negation (:var :y)))
;нужно избавиться от одиночных конъюнкций и дизъюнкций
(declare step6-expr)
(def step6-rules
  (list
    [(fn [expr] (and (conjunction? expr) (= (count (args expr)) 1)))
     (fn [expr] (second expr))]
    [(fn [expr] (and (disjunction? expr) (= (count (args expr)) 1)))
     (fn [expr] (second expr))]

    [(fn [expr] (negation? expr))
     (fn [expr] (negation (step6-expr  (second expr))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(step6-expr %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(step6-expr %) (args expr))))]
    [(fn [expr] (or (variable? expr) (const? expr)))
     (fn [expr] expr)]))
(defn step6-expr [expr]
  (compute expr step6-rules))
;---------------------------------------------------------------------
; теперь из этого (:conjunction (:negation (:var :y))) получаем это (:negation (:var :y))
(defn execute [var val expr]
  (->>
    expr
    step1-expr
    step2-expr
    step3-expr
    step4-expr
    (define-expr var val)
    step3-expr
    step5-expr
    step6-expr
    ))
(defn -main []
  (println (execute (variable :x) const-true (negation (negation (variable :x)))))
  (println (step1-expr (conjunction (variable :x) (variable :y))))
  (println (step1-expr (conjunction (negation (variable :x)) (variable :y))))
  (println (step1-expr (disjunction (negation (variable :x)) (variable :y))))
  (println (step1-expr (implication (negation (variable :x)) (variable :y))))

  (println (step2-expr (negation (conjunction (variable :x) (variable :y)))))
  (println (step3-expr (negation (negation const-false))))
  (println (step4-expr (conjunction (variable :z) (disjunction (variable :x) (variable :y)) )))
  (println (step4-expr (conjunction (disjunction (variable :x) (variable :y)) (variable :z))))
  (println (execute (variable :x) const-true (conjunction (variable :x) (variable :y))))
  (println (execute (variable :x) const-true (conjunction (negation (variable :x)) (variable :y))))
  (println (execute (variable :x) const-true (disjunction (negation (variable :x)) (variable :y))))
  (println (execute (variable :x) const-true (negation(disjunction (negation (variable :x)) (variable :y)))))
  (println (execute (variable :x) const-true (negation(conjunction (negation (variable :x)) (variable :y)))))
  )
