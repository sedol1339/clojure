(defn mean [x1 x2]
  (/ (+ x1 x2) 2))

(defn integral_part [func step step_number] ;от step*(step_number-1) до step*step_number
    (let [x2 (* step step_number) x1 (- x2 step)] (* step (mean (func x1) (func x2)))))

;(defn integral_rough [func step steps tmp]
;    (if (== steps 0) tmp (recur func step (- steps 1) (+ tmp (integral_part func step steps)))))

(defn integral [func]
  (let [step 1/100
        rough_values (atom [0])
        rough_values_swap_fn (fn [old_vec]
            (let [old_count (count old_vec)
                  last_elem (last old_vec)
                  delta (integral_part func step (- old_count 1))
                  value_to_append (+ last_elem delta)]
                (conj old_vec value_to_append)))]
    (fn [x] (let [steps (quot x step), tail (rem x step)]
      (while
          (<= (count @rough_values) (+ steps 1))
          (swap! rough_values rough_values_swap_fn))
	  (+ (nth @rough_values (+ steps 1)) (* tail (mean (func (- x tail)) (func x))))))))

; модульные тесты

(defn zero_fn [x] 0)
(defn x_fn [x] x)
(defn x_pow_2_fn [x] (* x x))

(def zero_fn_integral (integral zero_fn))
(def x_fn_integral (integral x_fn))
(def x_pow_2_fn_integral (integral x_pow_2_fn))

(defn almost_equal [x1 x2]
    (< -1/1000 (- x1 x2) 1/1000))

(println (zero_fn_integral 0))

(println (almost_equal 0 -0.00001))
(println (almost_equal 1.0 1.00001))
(println (almost_equal 0 (zero_fn_integral 0)))
(println (almost_equal 0 (zero_fn_integral 1)))
(println (almost_equal 0 (x_fn_integral 0)))
(println (almost_equal 1/2 (x_fn_integral 1)))
(println (almost_equal 1/3 (x_pow_2_fn_integral 1)))
(println (not= (x_fn_integral 1) (x_fn_integral 1.00001)))

; тесты производительности

(time (x_pow_2_fn_integral 10))
(time (x_pow_2_fn_integral 10))
(time (x_pow_2_fn_integral 9.9))
(time (x_pow_2_fn_integral 10.1))