(ns lab1.lab14)

(defn my-mapcat [f coll]
  (letfn [(func [acc x] (concat acc (f x)))]
    (reduce func nil coll)))

(defn func [symbols n]
  (nth (iterate (fn [combs]
    (letfn [(add-symbol [combination symbols] (map #(cons % combination) (filter #(not= (first combination) %) symbols) ))]
      (mapcat #(add-symbol % symbols) combs))) (list(list))) n))


(func (list "a" "b" "c" "d") 3)
