;; UPD: обновил код; взял код из 3.1 и добавил функцию parallel-filter-lazy
;; работает таким образом: лениво делит последовательность на блоки размером 10*n
;; каждый блок обрабатывает параллельно с помощью parallel-filter с параметром n
;; то есть каждый блок размером 10*n в дальнейшем делится на n частей, обрабатывающихся
;; параллельно. Все-таки думаю, что это не самое эффективное решение, так как потоки
;; постоянно создаются и уничтожаются, на эти операции тратятся ресурсы

;; 3.2. Реализуйте ленивый параллельный filter, который должен работать в том числе с бесконечными
;; потоками.

;; Подход к решению: в лекциях предлагается использовать pmap, однако среда repl.it зависает, что
;; говорит о бесконечном зацикливании. Не могу понять в чем проблема. Даже замена my-partition
;; на partition, который согласно документации является ленивым, не помогает, значит проблема не
;; в этом.

;; (defn my-partition 
;;   ([col n] (my-partition col n (vector)))
;;   ([col n tmp]
;;   (if
;;     (<= (count col) n)
;;     (conj tmp col)
;;     (recur (drop n col) n (conj tmp (take n col))))))

(defn my-partition [coll n]
; https://github.com/clojure/clojure/blob/clojure-1.10.1/src/clj/clojure/core.clj#L7012
  (lazy-seq
    (when-let [s (seq coll)]
      (let [p (doall (take n s))]
        (cons p (my-partition (nthrest s n) n))))))

(defn lazy-cat' [colls]
  (lazy-seq
    (if (seq colls)
      (concat (first colls) (lazy-cat' (next colls))))))

(defn parallel-filter [pred col n]
  (->> (my-partition col n)
       (map #(future (doall (filter pred %))))
       (doall)
       (map deref)
       ;(lazy-cat')
       (reduce concat)
       ))

(defn heavy-filter [value]
  (Thread/sleep 2)
  (odd? value))

(defn parallel-filter-lazy [pred col n]
  (->> 
       ;(partition (* 10 n) col)
       (my-partition col (* 10 n))
       (map #(parallel-filter pred % n))
       (lazy-cat')
       ))

; тесты

(println (=
  (doall (filter odd? (range 138)))
  (doall (parallel-filter odd? (range 138) 10))
  (doall (parallel-filter-lazy odd? (range 138) 10))))

(println (=
  (doall (filter odd? []))
  (doall (parallel-filter odd? [] 5))
  (doall (parallel-filter-lazy odd? [] 5))))

(println (=
  (doall (filter #(< % 10) (range 200)))
  (doall (parallel-filter #(< % 10) (range 200) 1))
  (doall (parallel-filter-lazy #(< % 10) (range 200) 1))))

; тесты производительности

(time (doall (filter heavy-filter (range 138))))
(time (doall (parallel-filter-lazy heavy-filter (range 138) 10)))

(time (doall (take 130 (filter heavy-filter (range)))))
(time (doall (take 130 (parallel-filter-lazy heavy-filter (range) 10))))

; проверка на переполнение стека

(dorun (take 100000 (parallel-filter-lazy odd? (range) 10)))