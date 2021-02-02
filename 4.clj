;; По аналогии с задачей дифференцирования реализовать представление символьных булевых выражений
;; с операциями конъюнкции, дизъюнкции отрицания, импликации. Выражения могут  включать как булевы
;; константы, так и переменные. Реализовать подстановку значения переменной в выражение с его
;; приведением к ДНФ. Обеспечить расширяемость для новых операций (исключающее ИЛИ, стрелка Пирса и пр.) 
;; Код должен быть покрыт тестами, API документирован.

;; Как я понимаю условие: импликацию можно сразу преобразовывать в дизъюнкцию по правилам логики,
;; Выражения можно сразу упрощать при конструировании. Одной из видов ДНФ является СДНФ, ее можно
;; строить по расчитанной таблице значений.

;; Расширяемость для новых операций обеспечивается там, что их можно представить в форме конъюнкций,
;; дизъюнкций и отрицаний, поэтому для создания новой операции достаточно создать новую функцию, но для
;; этого не нужно придумывать новые структуры данных.

;; Не старался добиться перфекционизма в тестировании, написал несколько десятков тестов. Не проверял
;; подстановку ошибочных аргументов в функции, проверял только корректную работу при корректных аргументах.

;; --------------------------------
;; константы и переменные
;; --------------------------------

(defn variable 
  "Создает переменную по имени. name - имя (keyword).
  Переменные с одним и тем же именем - одна и та же переменная"
  [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable?
  "Является ли аргумент переменной?"
  [expr]
  (= (first expr) ::var))

(defn variable-name
  "Получить имя переменной"
  [v]
  {:pre [(variable? v)]}
  (second v))

(defn same-variables?
  "Являются ли две переменные одной и той же, то есть имеют ли
  одно и то же имя?"
  [v1 v2]
  {:pre [(and (variable? v1) (variable? v2))]}
  (= (variable-name v1) (variable-name v2)))

(defn constant
  "Создание константы из keyword'ов :true или :false"
  [value]
  {:pre [(or (= :true value) (= :false value))]}
  (list ::const value))

(defn constant?
  "Является ли аргумент константой?"
  [expr]
  (= (first expr) ::const))

(defn constant-value
  "Получить значение переменной"
  [c]
  {:pre [(constant? c)]}
  (second c))

(defn is_true?
  "Является ли выражение константой со значением :true?"
  [expr]
  (and (constant? expr) (= (constant-value expr) :true)))

(defn is_false?
  "Является ли выражение константой со значением :false?"
  [expr]
  (and (constant? expr) (= (constant-value expr) :false)))

; тесты
(use '[clojure.test :only [is]])
(is (variable? (variable :x)))
(is (= :x (variable-name (variable :x))))
(is (same-variables?
  (variable :x)
  (variable :x)))
(is (not (same-variables?
  (variable :x)
  (variable :y))))
(is (constant? (constant :true)))
(is (= :true (constant-value (constant :true))))
(is (= :false (constant-value (constant :false))))

;; --------------------------------
;; операции
;; --------------------------------

(defn maybe-convert-keyword-to-var
  "Вспомогательная функция. Если аргумент является keyword'ом, конвертирует его в
  константу (если аргумент один из :true, :false) или переменную"
  [arg]
  (cond
    (= arg :true) (constant :true)
    (= arg :false) (constant :false)
    (keyword? arg) (variable arg)
    :else arg))

(defn convert-keywords-to-vars
  "Вспомогательная функция. Применяет maybe-convert-keyword-to-var
  к каждому элементу списка."
  [args]
  (map maybe-convert-keyword-to-var args))

(def ops {})

(defn op_and
  "Операция конъюнкции, принимает один или более аргументов (констант, переменных,
  выражений). Конвертирует аргументы, представленные keyword'ами, в константы
  или переменные. Осуществляет попытку упростить выражение по правилам логики:
  1. если среди аргументов есть (constant :true), удаляет эти аргументы
  2. если среди аргументов есть (constant :false), возвращает (constant :false)
  3. если аргумент всего один, возвращает его без изменений"
  [& args] 
  (let [args (convert-keywords-to-vars args) ;конвертируем keywords
        args (filter #(not (is_true? %)) args)] ;удаляем true
    (cond
      (some is_false? args) (constant :false)
      (= (count args) 1) (first args)
      :else (cons ::and args))))

(defn op_and?
  "Является ли аргумент конъюнкцией? Уточнение: если аргумент создан с помощью
  операции op_and, то он не обязательно является конъюнкцией, так как операция
  op_and пытается упростить выражение"
  [expr]
  (= ::and (first expr)))

(def ops (assoc ops op_and op_and?))

(defn op_or
  "Операция дизюнкции, принимает один или более аргументов (констант, переменных,
  выражений). Конвертирует аргументы, представленные keyword'ами, в константы
  или переменные. Осуществляет попытку упростить выражение по правилам логики:
  1. если среди аргументов есть (constant :false), удаляет эти аргументы
  2. если среди аргументов есть (constant :true), возвращает (constant :true)
  3. если аргумент всего один, возвращает его без изменений"
  [& args] 
  (let [args (convert-keywords-to-vars args) ;конвертируем keywords
        args (filter #(not (is_false? %)) args)] ;удаляем false
    (cond
      (some is_true? args) (constant :true)
      (= (count args) 1) (first args)
      :else (cons ::or args))))

(defn op_or?
  "Является ли аргумент дизъюнкцией? Уточнение: если аргумент создан с помощью
  операции op_or, то он не обязательно является дизъюнкцией, так как операция
  op_or пытается упростить выражение"
  [expr]
  (= ::or (first expr)))

(def ops (assoc ops op_or op_or?))

(defn op_not
  "Операция отрицания, принимает один аргумент (константу, переменную,
  выражение). Конвертирует аргумент, представленный keyword'ом, в константу
  или переменную. Осуществляет попытку упростить выражение по правилам логики:
  1. если аргумент равен (constant :false), возвращает (constant :true)
  2. если аргумент равен (constant :true), возвращает (constant :false)"
  [arg]
  (let [arg (maybe-convert-keyword-to-var arg)] ;конвертируем keywords
    (cond
      (is_true? arg) (constant :false)
      (is_false? arg) (constant :true)
      :else (list ::not arg))))

(defn op_not?
  "Является ли аргумент отрицанием? Уточнение: если аргумент создан с помощью
  операции op_not, то он не обязательно является отрицанием, так как операция
  op_not пытается упростить выражение"
  [expr]
  (= ::not (first expr)))

(def ops (assoc ops op_not op_not?))

(defn op_impl
  "Операция импликации, принимает два аргумента (константы, переменные,
  выражения). Вызывает op_or, осуществляя преобразование импликации
  в дизъюнкцию по правилам логики"
  [expr1 expr2]
  (op_or (op_not expr1) expr2))

(defn is_op?
  "Является ли аргумент операцией конъюнкции, дизъюнкции или отрицания?
  Уточнение: если аргумент создан с помощью одной из этих операций, то is_op не
  обязательно вернет true, поскольку операции осуществляют попытку упростить выражение"
  [expr]
  ;(or (op_not? expr) (op_or? expr) (op_and? expr)))
  (some true? (map #(% expr) (vals ops))))

; тесты
(is (op_and? (op_and (variable :x) (variable :y) (variable :z))))
(is (op_and? (op_and :x :y :z)))
(is (op_or? (op_or (variable :x) (variable :y) (variable :z))))
(is (op_or? (op_or :x :y :z)))
(is (op_or? (op_impl (variable :x) (variable :y))))
(is (op_or? (op_impl :x :y)))
(is (op_not? (op_not (variable :x))))
(is (op_not? (op_not :x)))
(is (is_false? (op_not :true)))
(is (is_true? (op_not :false)))

;проверяем упрощение:
(is (is_true? (op_not :false)))
(is (is_false? (op_not :true)))
(is (= :x (variable-name (op_and :x :true))))
(is (is_false? (op_and :x :false)))
(is (= :x (variable-name (op_or :x :false))))
(is (is_true? (op_or :x :true)))

;проверяем сложное выражение:
(let [expr (op_and :y (op_impl (op_and :x :y :true) (op_or :x :z :false)))]
  (list
    (is (is_op? expr))
    (is (op_and? expr))
    (is (not (op_or? expr)))
    (is (not (op_not? expr)))
  ))

;; --------------------------------
;; анализ выражений
;; --------------------------------

(defn get_args
  "Получить выражения-аргументы для данного выражения, являющегося операцией"
  [expr]
  {:pre [(is_op? expr)]}
  (rest expr))

(defn get-arg-list
  "Получить множество (set) используемых в выражении переменных в виде keyword'ов"
  [expr]
  (cond
    (constant? expr) #{}
    (variable? expr) #{(variable-name expr)}
    :else (apply clojure.set/union (map get-arg-list (get_args expr)))))

;; (defn pretty-print
;;   "Вспомогательная функция для отладки. Печатает выражение в виде строки"
;;   ([expr] (pretty-print expr true))
;;   ([expr print?]
;;     (let [string (cond
;;             (variable? expr) (str (variable-name expr))
;;             (constant? expr) (str (constant-value expr))
;;             (op_not? expr) (str "not " (pretty-print (first (get_args expr)) false))
;;             :else (let [arg-strings (map #(pretty-print % false) (get_args expr))
;;                         separator (cond (op_or? expr) " or " (op_and? expr) " and ")
;;                         arg-string-joined (clojure.string/join separator arg-strings)]
;;               (str "(" arg-string-joined ")")))]
;;       (if print? (println string) string))))

; тесты
(let [expr (op_or :y (op_impl (op_and :x :y :true) (op_or :x :z :false)))]
  (= #{:x :y :z} (get-arg-list expr)))

;; --------------------------------
;; подстановка значений переменных
;; --------------------------------

(defn substitute_single
  "Подстановка значения (value) переменной (_var) в выражение (expr). Переменная
  и значение могут быть представлены в форме variable/constant или keyword'ов.
  В процессе подстановки значения выражение может быть упрощено, хотя и не гарантируется
  его максимальное упрощение."
  [expr _var value]
  {:pre [(and (or (keyword? _var) (variable? _var)) (or (keyword? value) (constant? value)))]}
  (let [_var (maybe-convert-keyword-to-var _var)
        value (maybe-convert-keyword-to-var value)]
    (cond
      (constant? expr) expr
      (variable? expr) (if (same-variables? expr _var) value expr)
      :else (let [args_substituted (map #(substitute_single % _var value) (get_args expr))
	              _op (some identity (map (fn [[_op _op?]] (if (_op? expr) _op nil)) ops))]
          (apply _op args_substituted)
;         (cond
;            (op_not? expr) (apply op_not args_substituted)
;            (op_and? expr) (apply op_and args_substituted)
;            (op_or? expr) (apply op_or args_substituted)
;            :else (throw (Exception. "incorrect operation"))
 ;          )
          ))))

(defn substitute 
  "Подстановка значений одной или более переменных в выражение. Принимает нечетное
  количество аргументов: первый аргумент - выражение, далее пары (переменная, значение).
  Пары могут быть представлены в форме variable/constant или keyword'ов. В процессе
  подстановки значения выражение может быть упрощено, хотя и не гарантируется его
  максимальное упрощение. Пример: (substitute expr :x :true :y :false)"
  [expr _var value & others]
  (if
    (= 0 (count others))
    (substitute_single expr _var value)
    (substitute_single (apply substitute (cons expr others)) _var value)))

; тесты
(let [expr (op_or :x :y)]
  (list
    (is (variable? (substitute_single expr :x :false)))
    (is (= :y (variable-name (substitute_single expr :x :false))))
    (is (constant? (substitute_single expr :y :true)))
    (is (is_true? (substitute_single expr :y :true)))
    (is (is_true? (substitute_single (constant :true) :z :true)))
    (is (is_true? (substitute expr :x :false :y :true)))
    (is (is_false? (substitute expr :x :false :y :false)))
  ))

;; --------------------------------
;; дизъюнктивная нормальная форма
;; --------------------------------

(defn get_all_permutations
  "Вспомогательная функция. Получить все возможные сочетания из :true и :false указанной
  длины без повторений. Возвращает список из 2^len сочетаний, каждое в виде списка"
  [len]
  (nth (iterate #(for [start % end '(:true :false)] (conj start end)) (list (list))) len))

(defn calculate_single
  "Вспомогательная функция. Обертка над substitute, которая вместо чередующихся
  переменных и значений принимает два списка: в одном переменные, в другом значения.
  Пример: (calculate_single expr (:x :y) (:true :false))"
  [expr vars values]
  (apply substitute (conj (interleave vars values) expr)))

(defn calculate_multiple
  "Вспомогательная функция. Первые два аргумента - выражение и список имен переменных,
  values_list - список из списков значений. Работает как map, применяя к этому списку
  функцию calculate_single. Таким образом, принимая несколько вариантов значений аргументов,
  возвращает несколько результатов, полученных подстановкой аргументов в выражение"
  [expr vars values_list]
  (map #(calculate_single expr vars %) values_list))

(defn name-value-pair-to-expr
  "Вспомогательная функция. Принимает вектор из двух элементов: переменной и константы.
  Переменная и константа могут быть представлены в форме variable/constant или keyword'ов.
  Если константа равна :true, возвращает переменную, иначе отрицание переменной."
  [[_variable value]]
  (let [_variable (maybe-convert-keyword-to-var _variable)
        value (maybe-convert-keyword-to-var value)]
    (if
      (is_true? value)
      _variable
      (op_not _variable))))

(defn to_sdnf
  "Строит совершенную дизъюнктивную нормальную форму от выражения"
  [expr]
  (let [; Получаем список имен используемых в выражении переменных без повторений
        arg-names (apply list (get-arg-list expr))
        ; Получаем список всех возможных сочетаний значений переменных
        arg-values (get_all_permutations (count arg-names))
        ; С помощью calculate_multiple вычисляем результат (true или false) подстановкой
        ; всех возможных сочетаний значений переменных. Например, если в выражении N
        ; переменных, то получим 2^N сочетаний (arg-values) и 2^N результатов (output-values)
        output-values (map constant-value (calculate_multiple expr arg-names arg-values))
        ; Собираем arg-values и output-values в пары
        values-outputs-pairs (map vector arg-values output-values)
        ; Выбираем только те значения аргументов, для которых результат вычисления равен :true
        dnf-args (map first (filter #(= :true (last %)) values-outputs-pairs))
        ; Добавляем имена к аргументам
        dnf-args-with-names (map #(map vector arg-names %) dnf-args)
        ; Получаем список выражений, составляющих СДНФ
        dnf-exprs (map #(apply op_and (map name-value-pair-to-expr %)) dnf-args-with-names)
        ; Получаем СДНФ
        dnf (apply op_or dnf-exprs)]
    dnf))

; тесты
(let [expr (op_or :y (op_impl (op_and :x :y :true) (op_or :x :z :false (op_and :foo :false))))
      expr-sdnf (to_sdnf expr)
      arg-names (apply list (get-arg-list expr))
      arg-values (get_all_permutations (count arg-names))
      expr-output-values (map constant-value (calculate_multiple expr arg-names arg-values))
      expr-sdnf-output-values (map constant-value (calculate_multiple expr-sdnf arg-names arg-values))
      ]
  (list
    (is (= #{:x :y :z} (get-arg-list expr) (get-arg-list expr-sdnf)))
    (is (= 8 (count arg-values)))
    (is (= 8 (count expr-output-values)))
    (is (= 8 (count expr-sdnf-output-values)))
    ; проверяем равенство значений выражения и его СДНФ при всех возможных аргументах
    (is (= expr-output-values expr-sdnf-output-values))
  ))