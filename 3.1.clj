;; UPD:
;; заменил (filter pred %) на (doall (filter pred %))
;; таким образом вычисление фильтра полностью происходит во future, а не после deref

;; 3.1. Реализуйте параллельный вариант filter (не обязательно ленивый) с помощью
;; future. Параллельная обработка должна производиться блоками по заданному числу
;; элементов. Размер блоков следует вычислять вручную, без использования готовых
;; функций вроде partition (для разделения последовательности следует использовать
;; take и drop). Продемонстрируйте прирост производительности в сравнении с обычным
;; фильтром.

;; Подход к решению:
;; Создаю функцию my-partition, которая делит последовательность
;; на блоки из n элементов. Чтобы избежать переполнения стека, эта функция работает
;; по принципу хвостовой рекурсии, принимая дополнительный аргумент tmp - изначально
;; пустой вектор: берет n элементов из col и перекладывает их как один элемент в tmp,
;; а затем вызывает саму себя, передавая полученные результаты.
;; В функции  parallel-filter применяю my-partition, map с future, doall, map с
;; deref. Затем элементы нужно обратно разгруппировать, но apply concat вызывает
;; переполнение стека, поэтому вместо этого использую найденную в интернете функцию
;; lazy-cat', которая соединяет список из списков в один список.

;; Делаю как в лекциях: (map #(future ...)), затем (doall), затем (map deref), но
;; производительность по сравнению с однопоточной реализацией почему-то не растет.

(defn my-partition 
  ([col n] (my-partition col n (vector)))
  ([col n tmp]
  (if
    (<= (count col) n)
    (conj tmp col)
    (recur (drop n col) n (conj tmp (take n col))))))

; тест
; (my-partition (range 11) 3)
; (do (my-partition (range 100000) 3) :test-passed)

; обратное преобразование
; (reduce concat (my-partition (range 11) 3))

(defn lazy-cat' [colls]
  (lazy-seq
    (if (seq colls)
      (concat (first colls) (lazy-cat' (next colls))))))

(defn parallel-filter [pred col n]
  (->> (my-partition col n)
       (map #(future (doall (filter pred %))))
       (doall)
       (map deref)
       (lazy-cat')))

(defn heavy-filter [value]
  (Thread/sleep 10)
  (odd? value))

(time (doall (filter heavy-filter (range 100))))
(time (doall (parallel-filter heavy-filter (range 100) 10)))