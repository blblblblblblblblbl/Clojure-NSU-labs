(ns lab1.lab14)

(defn func [symbols n]                                      ;требуемая к реализации функция
  (if (not= n 0)                                            ;condition
    (my-loop (list(list)) symbols n)                                   ;true
    ()                                                      ;false
    ))

(defn my-loop [combinations symbols n]         ;данная функция нужна нам вместо цикла чтобы проделать шаг n раз и довести длину комбинация до n
  (if (= n 1)
    (next-gen-comb combinations symbols)
    (my-loop (next-gen-comb combinations symbols) symbols (dec n))))

(defn next-gen-comb [combinations symbols]  ;использует функцию add-symbol просто для всех слов а не для одного и выдает список новых комбинаций
  (letfn [(add-symbol-map [comb] (add-symbol comb symbols))]
    (mapcat add-symbol-map combinations)
    ))                  ;переделано для map ввел функцию которая с помощью map будет без рекурсии для всех комбинаций работать
;///////////////////////////////////////////mapcat самому
(defn add-symbol [combination symbols]             ; эта функция дописывает букву в начало слова и возвращает список слов например если подали (list  "d") (list "a") ;=> (("a" "d") ("b" "d") ("c" "d"))
  (letfn [(fltr [symb] (not= (first combination) symb))]
    (letfn [(add-map [filtred-symbs] (cons filtred-symbs combination))]
      (map add-map (filter fltr symbols)))
    )                                                       ;переделано на map
  )
;///////////////////////////////////////////iterate вместо чего-то
(func (list "a" "b" "c" "d") 3)
;//////////////////////////////////////////////////////////////////////////////////////
;все, дальше просто мои записи проверочные, когда прогу писал
;///////////////////////////////////////////////////////////////////////////////









(mapcat  (list 1 2 3))
(map reverse (list (list 1 2 3) (list 4 5 6)))              ;=> ((3 2 1) (6 5 4))
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


