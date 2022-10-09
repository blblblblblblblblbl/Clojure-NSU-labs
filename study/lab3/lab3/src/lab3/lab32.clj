(ns lab3.lab32)
;схема решения
;есть ленивый список
;мы его режем на чанки
;каждый чанк обрабатываеи один поток
;значит обрабатываем сразу n чанков , где n количество потоков
;n чанков это один batch

(defn parts [n coll]
  (map (partial take n) (iterate (partial drop n) coll)))

(defn parallel-filter-chunk [func coll chunk-size batch-size]
  (->>(parts chunk-size coll )
      (parts batch-size)
      (map (fn [batch]
             (->> (map #(future (doall(filter func %))) batch)
                  (doall batch-size)
                  (map deref))))
      (apply concat)
      (apply concat)
      ))



(defn parallel-filter
  ([func coll chunk-size ] (parallel-filter-chunk func coll chunk-size (.. Runtime (getRuntime) (availableProcessors)) ))
  ([func coll chunk-size threads] (parallel-filter-chunk func coll chunk-size threads )))
; количество потоков и будет batch-size

(defn heavy-even? [x]
  (Thread/sleep 100)
  (even? x))


(time (doall (take 30 (filter heavy-even? (iterate inc 0)))))
;"Elapsed time: 6389.2208 msecs"
(time (doall (take 50 (parallel-filter heavy-even? (iterate inc 0) 10 10))))
;"Elapsed time: 1092.7255 msecs"
(time (doall (take 50 (parallel-filter heavy-even? (iterate inc 0) 10))))
;"Elapsed time: 1089.3237 msecs"

