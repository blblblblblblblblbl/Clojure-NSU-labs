(ns lab2.lab2-1)
(-> 5 (* 6) (* 7))
(- 5 6)
; функция расчета площади трапеции
(defn trapezoid-area [f x1 x2]
  (-> (f x1) (+ (f x2)) (* 0.5) (* (- x2 x1))))

(defn integrate [f step]
  (let [integr (fn [integr f step x]  ;вот так делаем рекурсию что фунция принимает себя же в качестве аргумента
                 (if (> x 0)
                   (+ (trapezoid-area f (- x step) x) (integr integr f step (- x step)))
                   0)
                 )]
    (partial integr integr f step)
    ))



 (defn mem-integrate [f step]
   (let [integr (fn [integr f step x]
                  (if (> x 0)
                    (+ (trapezoid-area f (- x step) x) (integr integr f step (- x step)))
                    0)
                  )
         ]
     (partial integr (memoize integr) f step)
     ))
; получается тут у нас (memoize integr) эта функция уже составляет таблицу аргументов и результатов
; работает для разных точек потому что мы например сделали для x=100
; потом вызвали для x = 101 пойдет рекурсия и от 101 до 100 программа посчитает а где уже будет от x= 100 там возьмет ответ готоый


(trapezoid-area (fn [x] x) 0 3)


(defn func1 [x] x)
((integrate func1 0.5) 3)

(defn -main []
  (let [integral (integrate func1 0.5)
        integral-mem (memoize (integrate func1 0.5))
        mem-integral (mem-integrate func1 0.5)
        ]
    ;обычный интеграл
    (time (integral 100))
    (time (integral 100))
    (time (integral 100))
    ;"Elapsed time: 0.2808 msecs"
    ;"Elapsed time: 0.2003 msecs"
    ;"Elapsed time: 0.1769 msecs"

    ;обычный интеграл с мемоизацией его результата
    (time (integral-mem 100))
    (time (integral-mem 100))
    (time (integral-mem 100))
    (time (integral-mem 101))
    (time (integral-mem 102))
    ;"Elapsed time: 0.0892 msecs"
    ;"Elapsed time: 0.0093 msecs"
    ;"Elapsed time: 0.2812 msecs"
    ;"Elapsed time: 0.0841 msecs"

    ; мемный интеграл
    (time (mem-integral 100))
    (time (mem-integral 100))
    (time (mem-integral 101))
    (time (mem-integral 102))
    (time (mem-integral 99))
    ;""Elapsed time: 1.5701 msecs"
    ;"Elapsed time: 0.0365 msecs"
    ;"Elapsed time: 0.038 msecs"
    ;"Elapsed time: 0.02 msecs"
    ;"Elapsed time: 0.0111 msecs"

    ;сразу видно что мемы работают
    ;отличия integral-mem от mem-integral в том что
    ;integral-mem работает только для одной точки
    ;а mem-integral в разных
    )

  )

((partial + 1 2) 3)