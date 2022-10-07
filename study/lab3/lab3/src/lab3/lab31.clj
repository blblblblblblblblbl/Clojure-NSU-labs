(ns lab3.lab31)

(defn heavy-even? [x]
  (Thread/sleep 100)
  (even? x))
;можно вытащить количество потоков через функции из java
;сделать перегрузку на parallel-filter2, чтобы если два параметра передаем то программа сама
;берет количество потоков а если 3 то мы сообщаем об их количестве
;сделать чтобы передевать количество потоков
(defn make-parts-list [coll n]
  (if (empty? coll)
    ()
    (concat (list (take n coll)) (make-parts-list (drop n coll) n))
    ))
(defn connect-parts [coll]
  (if (empty? coll) ()  (concat (first coll) (connect-parts (rest coll))) ))
(defn parallel-filter2 [func coll n]
  (->>(make-parts-list coll n)
      (map #(future (doall (filter func %))))
      (doall)
      (map deref)
      ;(doall)
      (connect-parts)
      ))

(defn -main []
  (time (doall (filter heavy-even? (range 100))))
  ;"Elapsed time: 11032.4475 msecs"

  (time (doall (parallel-filter2 heavy-even? (range 100) 10)))
  ;"Elapsed time: 1100.1473 msecs" в 10 раз
  (time (doall (parallel-filter2 heavy-even? (range 100) 50)))
  ;"Elapsed time: 5538.7612 msecs" в 2 раза
  (time (doall (parallel-filter2 heavy-even? (range 100) 5)))
  ;"Elapsed time: 554.8963 msecs" в 20 раз
  (time (doall (parallel-filter2 heavy-even? (range 100) 1)))
  ;"Elapsed time: 144.3883 msecs" в ~100 раз
  )