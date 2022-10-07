(ns lab3.core)

(defn my-filter [f coll]
  (letfn [(func [acc x] (if (f x) (conj acc x) acc))]
    (reverse (reduce func nil coll))))
(defn parralel-filter [f coll]
  (let [acc (list)]
  (letfn [(make-parts [coll n]
            (if (empty? coll) (list) (concat acc (take n coll)))
            (make-parts (drop n coll) n)
            )]
    (make-parts (range 99) 10)
    )))
(parralel-filter 1 1)

(take 3 (list 1))
(drop 3 (list 1))
(empty? (drop 3 (list 1)))

(cons 1 (list (list )))
(defn heavy-inc [x] (delay 1000) (inc x))
(defn -main []
  (println (->> (iterate inc 0)
                (take 10)
                (map #(heavy-inc %))
                ))
  (time
  (->> (iterate inc 0)
       (take 10)
       (map #(heavy-inc %))
       ))
  (time
    (->> (iterate inc 0)
         (take 10)
         (map #(heavy-inc %))
         (doall)))

  (time
    (->> (iterate inc 0)
         (take 10)
         (map #(future (heavy-inc %)))
         (map deref)
         (doall)))
  (time
    (->> (iterate inc 0)
         (take 10)
         (map #(future (heavy-inc %)))
         (doall)
         (map deref)
         (doall)))


  )
