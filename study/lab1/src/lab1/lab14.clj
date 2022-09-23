(ns lab1.lab14)

(defn func [symbols n]                                      ;требуемая к реализации функция
  (if (not= n 0)                                            ;condition
    (my-loop (list(list)) symbols n)                                   ;true
    ()                                                      ;false
    ))

(defn func [symbols n]                                      ;требуемая к реализации функция
  (if (not= n 0)                                            ;condition
    (first (nth (iterate (fn [mass] (list ( next-gen-comb (first mass) symbols) (dec (last mass)))) (list (list(list)) n)) n))     ;true
    ()                                                      ;false
    ))
; как это работает мы должны подействовать next-gen-comb n раз на свой же вывод
; поэтому создаем конструкцию mass = (list comb n)
; делаем iterate который создаст нам список из которого нужно взять nth элемент чтобы получить вывод на определенной итерации
;функция для iterate берет mass на комб действует next-gen-comb а на n dec

(defn func [symbols n]                                      ;требуемая к реализации функция
  (if (not= n 0)                                            ;condition
    (nth (iterate (fn [combs] (next-gen-comb combs symbols) ) (list(list))) n)     ;true
    ()                                                      ;false
    ))

(defn func [symbols n]                                      ;требуемая к реализации функция
  (nth (iterate (fn [combs] (next-gen-comb combs symbols) ) (list(list))) n))
; а зачем нам вообще тогда n, он не нужен нам ,убрал его

(nth (iterate (fn [mass] (list ( next-gen-comb (first mass) symbols) (dec (last mass)))) (list (list(list)) n)) n)
; еще меньше стал код


(defn func [symbols n]                                      ;требуемая к реализации функция
  (nth (iterate (fn [combs] (
                              ;////////next-gen-comb
                             (fn [combinations symbols]  ;использует функцию add-symbol просто для всех слов а не для одного и выдает список новых комбинаций
                               (letfn [
                                       (add-symbol-map [comb] (add-symbol comb symbols))

                                       (add-symbol [combination symbols]             ; эта функция дописывает букву в начало слова и возвращает список слов например если подали (list  "d") (list "a") ;=> (("a" "d") ("b" "d") ("c" "d"))
                                         ;///////////////////////////////////////////mapcat самому
                                         (letfn [(fltr [symb] (not= (first combination) symb))]
                                           (letfn [(add-map [filtred-symbs] (cons filtred-symbs combination))]
                                             (map add-map (filter fltr symbols)))
                                           )                                                       ;переделано на map
                                         )

                                       ( my-mapcat [f coll]
                                         (letfn [(func [acc x] (concat acc (f x)))]
                                           (reduce func nil coll)))
                                       ]
                                 (my-mapcat add-symbol-map combinations)        ;замена mapcat
                                 ))
                              ;///////next-gen-comb
                              combs symbols) ) (list(list))) n))


(defn my-loop [combinations symbols n]         ;данная функция нужна нам вместо цикла чтобы проделать шаг n раз и довести длину комбинация до n
  (if (= n 1)
    (next-gen-comb combinations symbols)
    (my-loop (next-gen-comb combinations symbols) symbols (dec n))))

(defn next-gen-comb [combinations symbols]  ;использует функцию add-symbol просто для всех слов а не для одного и выдает список новых комбинаций
  (letfn [(add-symbol-map [comb] (add-symbol comb symbols))]
    (mapcat add-symbol-map combinations)
    ))                  ;переделано для mapcat ввел функцию которая с помощью map будет без рекурсии для всех комбинаций работать

(defn next-gen-comb [combinations symbols]  ;использует функцию add-symbol просто для всех слов а не для одного и выдает список новых комбинаций
  (letfn [(add-symbol-map [comb] (add-symbol comb symbols))]
    (apply concat (map add-symbol-map combinations))        ;замена mapcat
    ))

(defn my-mapcat [f coll]
  (letfn [(func [acc x] (concat acc (f x)))]
    (reduce func nil coll)))

(defn next-gen-comb [combinations symbols]  ;использует функцию add-symbol просто для всех слов а не для одного и выдает список новых комбинаций
  (letfn [(add-symbol-map [comb] (add-symbol comb symbols))]
    (my-mapcat add-symbol-map combinations)        ;замена mapcat
    ))

(defn add-symbol [combination symbols]             ; эта функция дописывает букву в начало слова и возвращает список слов например если подали (list  "d") (list "a") ;=> (("a" "d") ("b" "d") ("c" "d"))
;///////////////////////////////////////////mapcat самому
  (letfn [(fltr [symb] (not= (first combination) symb))]
    (letfn [(add-map [filtred-symbs] (cons filtred-symbs combination))]
      (map add-map (filter fltr symbols)))
    )                                                       ;переделано на map
  )

(my-mapcat reverse (list (list 1 2 3) (list 4 5 6)))     ;=> (3 2 1 6 5 4)

                                                                                                            ;

(func (list "a" "b" "c" "d") 3)
;//////////////////////////////////////////////////////////////////////////////////////
;все, дальше просто мои записи проверочные, когда прогу писал
;///////////////////////////////////////////////////////////////////////////////



(take 3 (iterate inc 5))        ;=> (5 6 7)
(take 3 (iterate  (fn [a] a) 5));=> (5 5 5)
(take 3 (iterate  (fn [a] 4) 5)) ;=> (5 4 4)
;то есть видно что iterate сначала ставит x на первое место, а потом уже начиная со второго элемента действует на предыдущий
(take 3 (iterate dec 3))     ;=> (3 2 1)
(nth (iterate dec 3) 2)
(take n (iterate dec n))




(mapcat  (list 1 2 3))
(map reverse (list (list 1 2 3) (list 4 5 6)))              ;=> ((3 2 1) (6 5 4))
(apply concat (map reverse (list (list 1 2 3) (list 4 5 6)))) ;=> (3 2 1 6 5 4)
(mapcat reverse (list (list 1 2 3) (list 4 5 6)))           ;=> (3 2 1 6 5 4)
(map add-symbol combinations symbols)
(map  + (list 1 2 3) (1))
(letfn [(add-symbol-map [comb] (add-symbol comb symbols))])

(next-gen-comb (list (list "a" "b") (list "a" "b")) (list "c" "d"))

(add-symbol (list  "d") (list "a" "b" "c" "d"))

(defn aaa [comb symbs]
  (letfn [(fltr [symb] (not= (first comb) symb))]
    (filter fltr symbs)
    ))
(aaa (list "a" "b") (list "a" "b" "c"))


