;; Задача 1.4. Изменить решение задачи 1.1/1.2 таким образом, чтобы вместо рекурсивных вызовов
;; использовались map/reduce/filter.

;; Как я понимаю условие: нельзя использовать рекурсивные вызовы, только map, reduce, filter

(defn list_with_element_product [col elem]
  (map #(list % elem) col))

(defn list_with_list_product [col1 col2]
  (reduce
    (fn [a b] (concat a (list_with_element_product col1 b)))
    (list)
    col2))

(defn get_sequences [symbols len]
  (let [concat_fn #(str (first %) (last %))
        test_fn #(not(.endsWith (first %) (str(last %))))]
    (reduce
      (fn [a b]
        (my-map (my-filter (list_with_list_product a symbols) test_fn) concat_fn))
      (list "")
      (range len))))

(get_sequences '(\a \b \c) 3)

;; условие задачи 1.4 выполнено
;; приведу еще один вариант решения задачи 1 с for и рекурсией:

(defn get_sequences [symbols len]
  (if
    (== len 0)
    (list "")
    (for [prefix (get_sequences symbols (- len 1))
          s symbols
          :when (not= (last prefix) s)]
      (str prefix s))))

(get_sequences '(\a \b \c) 3)

;; и аналогичный код на python

;; import itertools
;; def get_sequences(symbols, length):
;;   if length == 0:
;;     return [""]
;;   prefixes = get_sequences(symbols, length - 1)
;;   return [x + y for x in prefixes for y in symbols if not x.endswith(y)]

;; get_sequences('abc', 3)