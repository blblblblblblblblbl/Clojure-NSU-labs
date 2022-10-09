(ns lab3.lab31draft)
(ns study.lab3)
(->> (iterate inc 0)
     (take 10)
     (map inc)
     )

(->> (iterate inc 0)
     (take 10)
     (map #(future (inc %)))
     (map deref))

(defn heavy-inc [n]
  (Thread/sleep 100)
  (inc n))

(time
  (->> (iterate inc 0)
       (take 10)
       (map heavy-inc)
       (doall)))

(time
  (->> (iterate inc 0)
       (take 10)
       (filter heavy-even?)
       (doall)))

(defn make-parts-list [coll n]
  (if (empty? coll)
    ()
    (concat (list (take n coll)) (make-parts-list (drop n coll) n))
    ))

(make-parts-list (range 11) 10)

(defn parallel-filter [coll n]
  (->>(make-parts-list coll n)
      (map #(future (filter even? %)))
      (doall)
      (map deref)
      (doall)
      ))
(parallel-filter (range 100) 10)

(defn parallel-filter [func coll n]
  (->>(make-parts-list coll n)
      (map #(future (filter func %)))
      (doall)
      (map deref)
      (doall)
      ))

(parallel-filter even? (range 100) 10)

(list (list 1 2) (list 3 4))
(concat (first (list (list 1 2) (list 3 4))) (rest (list (list 1 2) (list 3 4))))

(defn connect-parts [coll]
  (if (empty? coll) ()  (concat (first coll) (connect-parts (rest coll))) ))
;в итоге получил такое
(defn parallel-filter [func coll n]
  (->>(make-parts-list coll n)
      (map #(future (filter func %)))
      (doall)
      (map deref)
      (doall)
      (connect-parts)
      ))
(parallel-filter even? (range 100) 10)
; сравнение
;долгий even
(defn heavy-even? [x]
  (Thread/sleep 100)
  (even? x))
; параллельный фильтр
(defn parallel-filter [func coll n]
  (->>(make-parts-list coll n)
      (map #(future (filter func %)))
      (doall)
      (map deref)
      (doall)
      (connect-parts)
      ))
(time (doall (parallel-filter heavy-even? (range 100) 10)))
;"Elapsed time: 11030.369 msecs"
(defn parallel-filter2 [func coll n]
  (->>(make-parts-list coll n)
      (map #(future (doall (filter func %))))
      (doall)
      (map deref)
      (doall)
      (connect-parts)
      ))
(time (doall (parallel-filter2 heavy-even? (range 100) 10)))
;"Elapsed time: 1100.1473 msecs" в 10 раз
(time (doall (parallel-filter2 heavy-even? (range 100) 50)))
;"Elapsed time: 5538.7612 msecs" в 2 раза
(time (doall (parallel-filter2 heavy-even? (range 100) 5)))
;"Elapsed time: 554.8963 msecs" в 20 раз
(time (doall (parallel-filter2 heavy-even? (range 100) 1)))
;"Elapsed time: 144.3883 msecs" в ~100 раз
(defn parts-filter [func coll n]
  (->>(make-parts-list coll n)
      (map #(filter func %))
      (connect-parts)
      ))
(time (doall (parts-filter heavy-even? (range 100) 10)))
;"Elapsed time: 11070.9823 msecs"
(time (doall (filter heavy-even? (range 100))))
;"Elapsed time: 11032.4475 msecs"


(iterate #(take 10) (range 100))

(connect-parts (list (list 1) (list 2)))
(rest (list (list 1) (list 2)))
(rest (rest (list (list 1) (list 2))))
(empty? (rest (rest (list (list 1) (list 2)))))

(first (rest (list (list 1) (list 2))))

((partial (filter even?)) (range 100))

(println (doall (map (partial filter even?) (make-parts-list (range 100) 10))))
(defn -main []
  (make-parts-list (list) 1)
  )

(defn parallel-filter-threads
  ([func coll] (let [coll-size (count coll)
                     cpu-cores (.. Runtime (getRuntime) (availableProcessors))]
                 (if (== (rem coll-size cpu-cores) 0)
                   (parallel-filter2 func coll (quot coll-size cpu-cores))
                   (parallel-filter2 func coll (+ 1 (quot coll-size cpu-cores))))))
  ([func coll n] (parallel-filter2 func coll (quot (count coll) n))))