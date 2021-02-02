;; Задача 1.3. Определить функции my-map и my-filter, аналогичные map (для одного списка)
;; и filter, выразив их через reduce и базовые операции над списками (cons, first, concat
;; и т.п.).

(defn my-filter [col condition]
  (reduce
    (fn [a b]
      (if (condition b) (conj a b) a))
    (list)
    col))

; тест
(my-filter (list_with_list_product '("aa" "bb" "cc") '(\a \b \c)) #(not(.endsWith (first %) (str(last %)))))

(defn my-map [col func]
  (reduce
    (fn [a b]
      (conj a (func b)))
    (list)
    col))

; тест
(my-map (list_with_list_product '("aa" "bb" "cc") '(\a \b \c)) #(str (first %) (last %)))