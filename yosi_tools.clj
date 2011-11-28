;;;
;;; Yosi tools
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prime sieve from rich hickey
;; http://paste.lisp.org/display/69952
(defn sieve [n]
  (let [n (int n)]
    "Returns a list of all primes from 2 to n"
    (let [root (int (Math/round (Math/floor (Math/sqrt n))))]
      (loop [i (int 3)
             a (int-array n)
             result (list 2)]
        (if (>= i n)
          (reverse result)
          (recur (+ i (int 2))
                 (if (< i root)
                   (loop [arr a
                          inc (+ i i)
                          j (* i i)]
                     (if (>= j n)
                       arr
                       (recur (do (aset arr j (int 1)) arr)
                              inc
                              (+ j inc))))
                   a)
                 (if (zero? (aget a i))
                   (conj result i)
                   result)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; create prime list
(let [prime-max 1000000
      prime-array (boolean-array prime-max false)
      prime-list  (sieve prime-max)]

  (doall (map #(aset prime-array % true) prime-list) )
  
  (defn is-prime? [n]
    (if (> n prime-max)
      false
      (aget prime-array n)))
    

  (defn prime-list-under [n]
    (if (> n prime-max)
      (do (println "Warning: I have no more than " prime-max " primes.")
	  prime-list)
      (take-while #(< % n) prime-list)))

  (defn get-prime-list
    "Returns prime list"
    []
    prime-list)

)
;;; old implementation
(let [*prime-list* (atom [])]

  (defn is-prime? 
    "Check if the given number is a prime num or not.
     It needs *prime-list* contains at least sqrt of the given number(target)."
    [target]
    (loop [plist @*prime-list*]
      (let [check-num (first plist)]
	(cond (empty? plist) true
	      (> check-num (Math/sqrt target)) true
	      (zero? (rem target check-num)) false
	      true (recur (rest plist))))))

  (defn create-prime-list-under 
    "Create *prime-list* contains prime numbers under given number(n)."
    [n]
    (loop [target 2]
      (if (>= target n)
	true
	(if (is-prime? target)
	  (do
	    (reset! *prime-list* (conj @*prime-list* target))
	    (recur (inc target)))
	  (recur (inc target))))))

  (defn prime-nums-under
    "Returns prime number list which contains nums under given number(n). n is not included."
    [n]
    (take-while #(< % n) @*prime-list*))

  
  (defn get-prime-list
    "Returns prime list"
    []
    @*prime-list*)

  (create-prime-list-under 1000000)
    
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; another prime functions

(defn is-prime-sub? [target prm-list]
  (zero? (count (filter #(zero? (rem target %))
            (take-while #(< % (Math/sqrt target)) prm-list)))))

(defn find-prime [n]
  (loop [prime-list '(2 3 5)
     target 6]
    (if (>= (count prime-list) n)
      prime-list
      (if (is-prime-sub? target prime-list)
    (recur (concat prime-list (list target)) (inc target))
    (recur prime-list (inc target))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; factors
(defn factors 
  "Returns a list of factors of n.
     ex. (foctors 12) => (2 2 3)
   This function needs prime nember tool."
  [n]
  (loop [factor-list ()
     target n
     prime-list (prime-nums-under target)]
    (if (or (= target 1) (empty? prime-list))
      (reverse factor-list)
      (let [one-prime (first prime-list)]
    (if (zero? (rem target one-prime))
      (recur (cons one-prime factor-list) (/ target one-prime) prime-list)
      (recur factor-list target (rest prime-list)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fusion
(defn fusion
"Returns a list that contains each items for its maximum count.
 ex. (fusion '(2 3 3) '(3 4 4)) => (2 3 3 4 4)"
  ([list1 list2]
     (fusion list1 list2 ()))
  ([list1 list2 res-list]
     (cond (empty? list1) (sort (concat res-list list2))
	   (empty? list2) (sort (concat res-list list1))
       true
       (let [top-of-1 (first list1)
	     top-of-2 (first list2)]
         (cond
	  (= top-of-1 top-of-2) (recur (rest list1) (rest list2) (cons top-of-1 res-list))
	  (< top-of-1 top-of-2) (recur (rest list1) list2 (cons top-of-1 res-list))
	  (> top-of-1 top-of-2) (recur list1 (rest list2) (cons top-of-2 res-list)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create memoised func
(defmacro defn-memo 
  "Creates memoised function.
   And also accessor of the memoized data which named <fname>-data."
  [name arg-list & body]
  `(let [mem# (atom {})]

     (defn ~(symbol (str (str name) "-data")) []
       @mem#)

     (defn ~(symbol (str (str name) "-clear")) []
       (reset! mem# {}))

     (defn ~name ~arg-list
       (if-let [e# (find @mem# ~arg-list)]
	 (val e#)
	 (let [ret# ~@body]
	   (swap! mem# assoc ~arg-list ret#)
	   ret#)))))


;; test
;;(defn-memo my-fib [n]
;;  (if (< n 2)
;;    n
;;    (+ (my-fib (- n 1))
;;       (my-fib (- n 2)))))
;;(time (my-fib 100))
;;first time  : "Elapsed time: 1.256584 msecs"
;;second time : "Elapsed time: 0.057828 msecs"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transpose
;;
;; [[ 1  2  3  4]
;;  [11 12 13 14]
;;  [21 22 23 24]
;;  [31 32 33 34]]
;;   -->
;; ((1 11 21 31)
;;  (2 12 22 32)
;;  (3 13 23 33)
;;  (4 14 24 34))
;; function ver
(defn transpose-grid 
  "Transpose grid.
     [[1 2]
      [3 4]] ->
     [[1 3]
      [2 4]]"
  [grid]
  (apply map list grid))
;; macro ver
(defmacro transpose-grid [grid]
  `(map list ~@grid))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grouping
(defn group-same
  "Split into same value group
    ex. (2 3 1 3 1 3 3) -> [(1 1) (2) (3 3 3 3)]"
  ([col] (group-by-same (sort col) []))
  ([col res]
     (if (empty? col)
       res
       (let [[head tail] (split-with #(= (first col) %) col)]
	 (recur tail (conj res head ))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; all devisor
(defn all-divisor [n]
  (sort >
	(map #(apply * %)
	     (for-of-all (map calc-multi (group-by-same (factors n)))))))

(defn all-divisor2 [n]
  (filter #(= (rem n %) 0) (range n 0 -1)))

(defn paird-divisor [n]
  (filter #(not= 0 %)
	  (map #(if (zero? (rem n %))
		  [% (/ n %)]
		  0)
	       (range  (ceil (sqrt n)) (inc n)))))


(defn calc-multi [lst]
  ;; (2 2 2) -> (8 4 2 1)
  (if (empty? lst)
    [1]
    (cons (apply * lst) (calc-multi (rest lst)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi combination
(defn multi-combination
  "returns collection list of each one item of collectinos
   ex (get-all-of-for '([1 2 3] [4] [6 7])) -> ([6 4 1] [7 4 1] [6 4 2] [7 4 2] [6 4 3] [7 4 3])"
  [col]
     (if (empty? col)
       []
       (for [x (first col) y (get-all-of-for (rest col))] (cons x y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; triangle, pentagonal, hexagonal, and so on.
;; 3
(defn triangle-num [n]
  (/ (* n (+ 1 n)) 2))

(defn triangle-num? [n]
  (zero? (rem (- (Math/sqrt (+ (* 8 n) 1)) 1) 2)))

;; 4
(defn square-num [n]
  (* n n))

(defn square-num? [n]
  (= (math/expt (int (Math/sqrt n)) 2) n))

;; 5
(defn pentagonal [n]
  (/ (* n (- (* 3 n) 1)) 2))

(defn pentagonal? [n]
  (zero? (rem (+ 1 (Math/sqrt (+ 1 (* 24 n)))) 6)))

;; 6
(defn hexagonal [n]
  (* n (- (* 2 n) 1)))

(defn hexagonal? [n]
  (zero? (rem (+ 1 (Math/sqrt (+ 1 (* 8 n)))) 4)))

;; 7
(defn heptagonal [n]
  (/ (* n (- (* 5 n) 3)) 2))

(defn heptagonal? [n]
  (zero? (rem (+ 3 (Math/sqrt (+ 9 (* 40 n)))) 10)))

;; 8
(defn octagonal [n]
  (* n (- (* 3 n) 2)))

(defn octagonal? [n]
  (zero? (rem (+ 1 (Math/sqrt (+ 1 (* 3 n)))) 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list <-> num
(defn list-to-num [digit-list]
  (apply + (map #(* %1 %2) (reverse digit-list) (iterate #(* 10 %) 1))))

(defn num-to-list [num]
  (map #(Character/digit % 10) (str num)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sort by its count
(defn sort-by-count [num-list]
  "Sort num. set letf according to the count of the number.
   ex. [2 2 3 7 7 7 8] -> (7 2 8 3) "
  (map first
       (sort (fn [a b]
	       (if (= (count a) (count b))
		 (> (first a) (first b))
		 (> (count a) (count b))))
	     (group-same num-list))))



