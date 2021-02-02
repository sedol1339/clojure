;; UPD:
;; написал функцию my-iterate, заново проверил работу всей программы на тестах

;; Задача 2.2: численное интегрирование с потоками. Модифицировать решение задачи 2.1
;; таким образом, чтобы вместо мемоизации использовались потоки. Показать прирост
;; производительности с помощью time. Обеспечить покрытие функциональными тестами.

;; Подход к решению: вместо функции integral_rough (см. задачу 2.1) использую
;; ленивую последовательность на основе функции my-iterate; в нужный момент беру i-й
;; элемент этой последовательности.

(defn mean [x1 x2]
  (/ (+ x1 x2) 2))

(defn integral_part [func step step_number]
    (let [x2 (* step step_number) x1 (- x2 step)] (* step (mean (func x1) (func x2)))))

(defn my-iterate [func elem]
	(lazy-seq (cons elem (iterate func (func elem)))))

(defn integral [func]
  (let [step 1/1000
	    get_next_tuple (fn [[step_number sum]]
	                       [(inc step_number) (+ (integral_part func step (inc step_number)) sum)])
        integral_rough_tuples (my-iterate get_next_tuple [0 0]) ; пары: частичная сумма и номер шага
	    integral_rough (map last integral_rough_tuples)] ; только частичные суммы
    (fn [x] (let [steps (quot x step), tail (rem x step)]
	  (+ (nth integral_rough steps) (* tail (mean (func (- x tail)) (func x))))))))

; модульные тесты

(defn zero_fn [x] 0)
(defn x_fn [x] x)
(defn x_pow_2_fn [x] (* x x))

(def zero_fn_integral (integral zero_fn))
(def x_fn_integral (integral x_fn))
(def x_pow_2_fn_integral (integral x_pow_2_fn))

(defn almost_equal [x1 x2]
    (< -1/1000 (- x1 x2) 1/1000))

(println (almost_equal 0 -0.00001))
(println (almost_equal 1.0 1.00001))
(println (almost_equal 0 (zero_fn_integral 0)))
(println (almost_equal 0 (zero_fn_integral 1)))
(println (almost_equal 0 (x_fn_integral 0)))
(println (almost_equal 1/2 (x_fn_integral 1)))
(println (almost_equal 1/3 (x_pow_2_fn_integral 1)))
(println (not= (x_fn_integral 1) (x_fn_integral 1.00001)))

(time (x_pow_2_fn_integral 500))
(time (x_pow_2_fn_integral 500))
(time (x_pow_2_fn_integral 501))