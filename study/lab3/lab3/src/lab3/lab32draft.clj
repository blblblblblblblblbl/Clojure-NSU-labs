(ns lab3.lab32draft)
(iterate inc 0)
(take 10 (iterate inc 0))
(iterate #(drop 1 %) coll)
(iterate #(drop 1 %) (take 10 (iterate inc 0)))
(nth (iterate #(drop 1 %) (take 10 (iterate inc 0))) 3)
;написать свой ленивый partition через split-at
1 2 3 4 5 6 7 8 9
(1 2) (3 4) (5 6) (7 8) 9                                   ;чанки- разбиение данных
((1 2) (3 4) (5 6)) ((7 8) 9        )                            ;бэтчи разбиение задач тут у нас 3 чанка сразу будет борабатываться
; то есть если захотим только первый элемент отфильтрвать то нужно будет сделать все 6 на 3 потоках
(split-at)
(iterate (partial split-at 2) (range 100))
(take 4 (iterate (partial split-at 2) (range 10)))
(iterate inc 0)
(take 10 (iterate inc 0))
(take 10 (split-at 2 (iterate inc 0)))

(take 10 (iterate inc 0))
(take 10 (drop 10 (iterate inc 0)))

(let [coll (iterate inc 0)] ())
(take 3 (iterate (drop 10) (iterate inc 0)))
(take 10 (nth (iterate (partial drop 10) (iterate inc 0)) 0))

(map (partial take 10) (iterate (partial drop 10) (iterate inc 0)))

(nth (map (partial take 10) (iterate (partial drop 10) (iterate inc 0))) 2)

(list (iterate inc 0) (iterate inc 0) (iterate inc 0))
(take 10 (nth (list (iterate inc 0) (iterate inc 0) (iterate inc 0)) 1))
;сделать список в котором мы будем отбрасывать по n элементов
; (0 1 2...) (3 4 5...) (6 7 8...) тут по n=3
;и брать из него по n элеметов
(map (partial take n) (iterate (partial drop n) (iterate inc 0)))
;получаем такую ленивую штуку
; из нее берем через nth нужные элементы

(defn parts [coll n]
  (map (partial take n) (iterate (partial drop n) coll)))

(nth (parts (iterate inc 0) 10) 1)
(nth (parts (range 100) 10) 9)
(take 11 (parts (range 100) 11))

; работает
(defn parallel-filter2 [func coll n]
  (->>(parts coll n)
      (map #(future (filter func %)))
      (map deref)
      ))
(take 10 (parallel-filter2 even? (iterate inc 0) 10))
(apply concat (parallel-filter2 even? (iterate inc 0) 10))
(take 50 (apply concat (parallel-filter2 even? (iterate inc 0) 10)))

(defn parallel-filter2 [func coll n]
  (->>(parts coll n)
      (map #(future (filter func %)))
      (map deref)
      (apply concat)
      ))
(take 10 (parallel-filter2 even? (iterate inc 0) 10))

;----------------------------------------------------------- улучшение
(defn parallel-filter2 [func coll chunk-size batch-size]
  (->>(parts coll chunk-size)
      (map #(future (doall(filter func %))))
      (doall batch-size)
      (map deref)
      (apply concat)
      ))
;(doall batch-size) нужен иначе просто для doall
;не получится так как у нас бесконечный список там

;теперь надо придумать как сделать много batchей
(defn parallel-filter2 [func coll chunk-size batch-size]
  (->>(parts coll chunk-size)
      (map #(future (doall(filter func %))))
      (doall batch-size)
      (map deref)
      (apply concat)
      ))

(defn parallel-filter2 [func coll chunk-size batch-size]
  (->>(parts chunk-size coll )
      (parts batch-size)
      (map (fn [batch]
             (->> (map #(future (doall(filter func %))) batch)
                  (doall batch-size)
                  (map deref))))
      (apply concat)
      (apply concat)
      ))
; получилось такое

(defn parallel-filter-threads [func coll batch-size threads]
  (let [coll-size (count coll)]
    (if (== (rem coll-size threads) 0)
      (parallel-filter-chunk func coll batch-size (quot coll-size threads))
      (parallel-filter-chunk func coll batch-size (+ 1 (quot coll-size threads)))))
  )

(defn parallel-filter
  ([func coll batch-size ] (parallel-filter-threads func coll batch-size (.. Runtime (getRuntime) (availableProcessors)) ))
  ([func coll batch-size threads] (parallel-filter-threads func coll batch-size threads )))