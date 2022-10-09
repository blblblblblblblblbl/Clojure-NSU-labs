(ns lab3.lab31
  (:import (java.lang.management ManagementFactory)))

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
(defn parallel-filter-chunk [func coll n]
  (->>(make-parts-list coll n)
      (map #(future (doall (filter func %))))
      (doall)
      (map deref)
      ;(doall)
      (connect-parts)
      ))
;(.. ManagementFactory (getThreadMXBean) (getThreadCount))
;(Thread/activeCount)
;выдаю количество потоков которые используются в программе
;оба меняются во время исполнения программы
;(.. Runtime (getRuntime) (availableProcessors))
;выдает количество ядер у процессора
;не меняется
(== 2 1)
(defn parallel-filter-threads [func coll  threads]
  (let [coll-size (count coll)]
    (if (== (rem coll-size threads) 0)
      (parallel-filter-chunk func coll  (quot coll-size threads))
      (parallel-filter-chunk func coll  (+ 1 (quot coll-size threads)))))
  )

(defn parallel-filter
  ([func coll  ] (parallel-filter-threads func coll  (.. Runtime (getRuntime) (availableProcessors)) ))
  ([func coll threads] (parallel-filter-threads func coll  threads )))

(defn -main []
  (time (doall (filter heavy-even? (range 100))))
  ;"Elapsed time: 11032.4475 msecs"
  (time (doall (parallel-filter heavy-even? (range 100))))
  ;"Elapsed time: 1409.9375 msecs"
  (time (doall (parallel-filter heavy-even? (range 100) 100)))
  ;"Elapsed time: 137.4453 msecs"

  )