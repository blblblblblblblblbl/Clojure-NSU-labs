(ns lab2.lab2-2)
; функция расчета площади трапеции
(defn trapezoid-area [f x1 x2]
  (-> (f x1) (+ (f x2)) (* 0.5) (* (- x2 x1))))

(defn my-lazy-seq [f step]
      (let [integr (fn [x1] (trapezoid-area f x1 (+ x1 step)))
            steps (iterate (fn [x] (+ x step)) 0)]
           (reductions + 0 (map integr steps))
           )
      )
; получили ленивую последовательность интегрированных промежутков
; первый элемент интеграл от 0 до step
; второй от 0 до 2step
; третий от 0 до 3step и так далее


(defn integrate [f step]
      (let [my-seq (my-lazy-seq f step)]
           (fn [x] (nth my-seq (quot x step)))))

;берем из это последовательности нужный элемент


(defn func1 [x] (Thread/sleep 1) x)
(defn -main []
      (let [integral (integrate func1 0.5)
            my-seq (my-lazy-seq func1 0.5)
            ]
           (time (nth (my-lazy-seq func1 0.5) (quot 100 0.5)))
           ;"Elapsed time: 0.8705 msecs"
           (time (nth (my-lazy-seq func1 0.5) (quot 100 0.5)))
           ;"Elapsed time: 0.6754 msecs"
           (time (nth (my-lazy-seq func1 0.5) (quot 100 0.5)))
           (time (nth (my-lazy-seq func1 0.5) (quot 100 0.5)))
           (println "-----------------------")
           (time (nth my-seq (quot 100 0.5)))
           (time (nth my-seq (quot 100 0.5)))
           (time (nth my-seq (quot 105 0.5)))
           (println "-----------------------")

           (time (my-lazy-seq func1 0.5))
           ;"Elapsed time: 0.0065 msecs"

           (time (integral 100))
           (time (integral 100))
           (time (integral 200))
           (time (integral 101))
           (time (integral 300))
           (time (integral 105))
           (time (integral 99))
           ;"Elapsed time: 0.7693 msecs"
           ;"Elapsed time: 0.0997 msecs"
           ;"Elapsed time: 0.0845 msecs"
           ;"Elapsed time: 0.1322 msecs"
           ;"Elapsed time: 0.0348 msecs"
           ;"Elapsed time: 0.0205 msecs"
           )

      )