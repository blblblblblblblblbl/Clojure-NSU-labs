(ns lab1.lab12)
;1.2. Перепишите программу 1.1. так, чтобы все рекурсивные вызовы были хвостовыми

(defn func [symbols n]                                      ;требуемая к реализации функция
  (if (not= n 0)                                            ;condition
    (my-loop (list(list)) symbols n)                                   ;true
    ()                                                      ;false
    ))

(defn my-loop [combinations symbols n]         ;данная функция нужна нам вместо цикла чтобы проделать шаг n раз и довести длину комбинация до n
  (if (= n 1)
    (next-gen-comb combinations symbols)
    (recur (next-gen-comb combinations symbols) symbols (dec n)))) ;переделано на хвостовую



;смысл вводить Loop здесь чтобы ввести еще одно накопительную переменную то есть мы тут просто пробегаемся по combinations формируя при этом новый список new combinations c помощью add-symbol
(defn next-gen-comb [combinations symbols]  ;использует функцию add-symbol просто для всех слов а не для одного и выдает список новых комбинаций
  (loop [combinations-loop combinations  new-combinations-loop (list)]
    (if (empty? combinations-loop)                          ;condition
      new-combinations-loop              ;true
      (recur (rest combinations-loop) (concat new-combinations-loop (add-symbol (first combinations-loop) symbols))) ;переделано на loop  ;false
      ))
    )
;////без loop
(defn next-gen-comb [combinations symbols]  ;использует функцию add-symbol просто для всех слов а не для одного и выдает список новых комбинаций
  ((fn [combinations-loop new-combinations-loop]
     (if (empty? combinations-loop)                          ;condition
       new-combinations-loop              ;true
       (recur (rest combinations-loop) (concat new-combinations-loop (add-symbol (first combinations-loop) symbols))))) combinations (list))
  )


(defn add-symbol [combination symbols]             ; эта функция дописывает букву в начало слова и возвращает список слов например если подали (list  "d") (list "a") ;=> (("a" "d") ("b" "d") ("c" "d"))
  (loop [combination-loop combination symbols-loop symbols combinations-loop (list)] ; вводим loop который становится целью recur вводим его и переменные новые
    (if (empty? symbols-loop)                                      ;condition
      combinations-loop                                                      ;true
      (if (not= (first combination) (first symbols-loop))        ;false and new cond
        (recur combination-loop (rest symbols-loop) (cons (cons (first symbols-loop) combination-loop) combinations-loop) )      ;true
        (recur combination-loop (rest symbols-loop) combinations-loop)                                       ;false
        )
      )
    )
  )

;////без loop
(defn add-symbol [combination symbols]             ; эта функция дописывает букву в начало слова и возвращает список слов например если подали (list  "d") (list "a") ;=> (("a" "d") ("b" "d") ("c" "d"))
  ((fn [ symbols-loop combinations-loop ] ; вводим loop который становится целью recur вводим его и переменные новые
    (if (empty? symbols-loop)                                      ;condition
      combinations-loop                                                      ;true
      (if (not= (first combination) (first symbols-loop))        ;false and new cond
        (recur  (rest symbols-loop) (cons (cons (first symbols-loop) combination) combinations-loop) )      ;true
        (recur  (rest symbols-loop) combinations-loop)                                       ;false
        )
      )
    ) symbols (list))
  )

(func (list "a" "b" "c") 2)