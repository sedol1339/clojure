;; Задача 1.1.
;; Условие: Задан набор (алфавит) символов в виде списка и число n. Опишите функцию,
;; которая возвращает список всех строк длины n, состоящих из этих символов и не
;; содержащих двух одинаковых символов, идущих подряд. Для решения использовать рекурсию
;; и базовые операции над константами и списками (str, cons, .concat и т.п.).

;; Как я понимаю условие: нельзя использовать map, reduce, filter, for и любые другие
;; управляющие конструкции, кроме рекурсивного вызова функций

;; План действий: пишу собственную функцию декартова произведения списков, пишу
;; собственные map и filter, затем применяю их. В задаче 1.3 также требуется написать
;; собственные map и filter, но отличие в том, что там нужно использовать reduce,
;; а в задаче 1.1 только рекурсивные вызовы. Таким образом условие задачи 1.1 будет выполнено.

(defn list_with_element_product [col elem]
  (if
    (== (count col) 0)
    (list)
    (conj (list_with_element_product (butlast col) elem) (list (last col) elem))))

(defn list_with_list_product [col1 col2]
  (if
    (== (count col2) 0)
    (list)
    (concat (list_with_list_product col1 (butlast col2)) (list_with_element_product col1 (last col2)))))

(defn my-filter [col condition]
  (if
    (== (count col) 0)
    (list)
    (let [elem (last col),
          prev_elems (my-filter (butlast col) condition)]
      (if (condition elem) (conj prev_elems elem) prev_elems))))

(defn my-map [col func]
  (if
    (== (count col) 0)
    (list)
    (conj (my-map (butlast col) func) (func (last col)))))

(defn get_sequences [symbols len]
  (if
    (== len 0)
    (list "")
    (let [prefixes (get_sequences symbols (- len 1))
          product (list_with_list_product prefixes symbols)
          concat_fn #(str (first %) (last %))
          test_fn #(not(.endsWith (first %) (str(last %))))]
      (my-map (my-filter product test_fn) concat_fn))))

(get_sequences '(\a \b \c) 3)