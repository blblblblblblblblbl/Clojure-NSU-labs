(ns lab1.lab12)
;1.2. Перепишите программу 1.1. так, чтобы все рекурсивные вызовы были хвостовыми

(defn add-symbol [combination symbols]
  ((fn [ symbols-loop combinations-loop ]
     (if (empty? symbols-loop)
       combinations-loop
       (if (not= (first combination) (first symbols-loop))
         (recur  (rest symbols-loop) (cons (cons (first symbols-loop) combination) combinations-loop) )
         (recur  (rest symbols-loop) combinations-loop)
         )
       )
     ) symbols (list))
  )

(defn next-gen-comb [combinations symbols]
  ((fn [combinations-loop new-combinations-loop]
     (if (empty? combinations-loop)
       new-combinations-loop
       (recur (rest combinations-loop) (concat new-combinations-loop (add-symbol (first combinations-loop) symbols))))) combinations (list))
  )


(defn my-loop [combinations symbols n]
  (if (= n 0)
    combinations
    (recur (next-gen-comb combinations symbols) symbols (dec n))))


(defn func [symbols n]
  (if (not= n 0)
    (my-loop (list(list)) symbols n)
    ()
    ))



