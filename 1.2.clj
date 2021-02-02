;; Задача 1.2.
;; Условие: Изменить решение задачи 1.1 таким образом, чтобы все рекурсивные вызовы были
;; хвостовыми.

;; План действий: в задаче 1.1 я использовал 5 рекурсивных функций, переписываю каждую
;; из них, используя recur.

(defn list_with_element_product
  ([col elem] (list_with_element_product col elem (list)))
  ([col elem intermediate_result]
    (if
      (== (count col) 0)
      intermediate_result
      (let [intermediate_result_updated (conj intermediate_result (list (last col) elem))]
        (recur (butlast col) elem intermediate_result_updated)))))

(defn list_with_list_product
  ([col1 col2] (list_with_list_product col1 col2 (list)))
  ([col1 col2 intermediate_result]
    (if
      (== (count col2) 0)
      intermediate_result
      (let [intermediate_result_updated (concat intermediate_result (list_with_element_product col1 (last col2)))]
        (recur col1 (butlast col2) intermediate_result_updated)))))

(defn my-filter
  ([col condition] (my-filter col condition (list)))
  ([col condition intermediate_result]
    (if
      (== (count col) 0)
      intermediate_result
      (let [elem (last col),
            intermediate_result_updated (if (condition elem) (conj intermediate_result elem) intermediate_result)]
        (recur (butlast col) condition intermediate_result_updated)))))

(defn my-map
  ([col func] (my-map col func (list)))
  ([col func intermediate_result]
    (if
      (== (count col) 0)
      intermediate_result
      (let [elem (last col),
            intermediate_result_updated (conj intermediate_result (func elem))]
        (recur (butlast col) func intermediate_result_updated)))))

(defn get_sequences
  ([symbols len] (get_sequences symbols len (list "")))
  ([symbols len prefixes]
    (if
      (== len 0)
      prefixes
      (let [concat_fn #(str (first %) (last %))
            test_fn #(not(.endsWith (first %) (str(last %))))
            product (list_with_list_product prefixes symbols)
            prefixes_updated (my-map (my-filter product test_fn) concat_fn)]
        (recur symbols (- len 1) prefixes_updated)))))

(get_sequences '(\a \b \c) 3)