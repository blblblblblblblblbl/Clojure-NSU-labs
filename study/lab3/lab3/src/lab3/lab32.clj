(ns lab3.lab32)
;схема решения
;есть ленивый список
;мы его режем на чанки
;каждый чанк обрабатываеи один поток
;значит обрабатываем сразу n чанков , где n количество потоков
;n чанков это один batch

(defn parts [n coll]
      (when (not-empty coll)
        (lazy-seq
          (cons (take n coll)
                (parts n (drop n coll))
                    ))))

(defn my-parallel-filter [func coll chunk-size threads]
  (mapcat deref
          (mapcat
            #(doall (map (fn [x] (future (doall (filter func x)))) %))
            (map #(parts chunk-size %)
                 (parts (* threads chunk-size) coll)
                 )
            ))
  )

(defn parallel-filter
  ([func coll chunk-size ] (my-parallel-filter func coll chunk-size (.. Runtime (getRuntime) (availableProcessors)) ))
  ([func coll chunk-size threads] (my-parallel-filter func coll chunk-size threads )))
; количество потоков и будет batch-size

(defn heavy-even? [x]
  (Thread/sleep 100)
  (even? x))

(time (doall (take 30 (my-parallel-filter heavy-even? (iterate inc 0) 5 6))))
(time (doall (take 50 (my-parallel-filter heavy-even? (list 1 2 3 4 5 6 7) 5 6))))

(time (doall (take 30 (filter heavy-even? (iterate inc 0)))))
;"Elapsed time: 6389.2208 msecs"
(time (doall (take 50 (parallel-filter heavy-even? (iterate inc 0) 10 10))))
;"Elapsed time: 1092.7255 msecs"
(time (doall (take 50 (parallel-filter heavy-even? (iterate inc 0) 10))))
;"Elapsed time: 1089.3237 msecs"

