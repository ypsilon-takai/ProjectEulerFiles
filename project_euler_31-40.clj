;;;
;;;  Project euler  31-40
;;;             Author : Yosi
;;;             From   : 2011/5/16
;;;

;; primes
;; prime list
(def *prime-list* (atom []))

(def is-prime?
     (memoize 
      (fn [target]
	(loop [plist @*prime-list*]
	  (let [check-num (first plist)]
	    (cond (empty? plist) true
		  (> check-num (Math/sqrt target)) true
		  (zero? (rem target check-num)) false
		  true (recur (rest plist))))))))

(defn create-prime-list-under [n]
  (loop [target 2]
    (if (>= target n)
      true
      (if (is-prime? target)
        (do
          (reset! *prime-list* (conj @*prime-list* target))
          (recur (inc target)))
        (recur (inc target))))))

(create-prime-list-under 1000000)
;"Elapsed time: 5736.586354 msecs"


;; factors 
(defn factors [n]
  ;; (foctors 12) => (2 2 3)
  (loop [factor-list ()
         target n
         prime-list @*prime-list*]
    (if (or (= target 1) (empty? prime-list))
      (reverse factor-list)
      (let [one-prime (first prime-list)]
        (if (zero? (rem target one-prime))
          (recur (cons one-prime factor-list) (/ target one-prime) prime-list)
          (recur factor-list target (rest prime-list)))))))





;; Problem 31  : 2011/5/16
;; "Elapsed time: 604.414248 msecs"
;;data structure
(defstruct coin-table
  :rest-amount
  :coin-list)

(defn next-list [in-table coin]
  (for [num (take-while #(<= (* % coin) (:rest-amount in-table)) (iterate inc 0))]
    (struct coin-table
     (- (:rest-amount in-table) (* coin num))
     (assoc (:coin-list in-table) coin num))))
     
(loop  [mid-flow (list (struct coin-table 200 {}))
	coin-list [200 100 50 20 10 5 2]
	result-list ()]
  (if (empty? coin-list)
    (concat result-list mid-flow)
    (let [new-list (mapcat #(next-list % (first coin-list)) mid-flow)]
      (recur (filter #(> (:rest-amount %) 0) new-list)
	     (rest coin-list)
	     (concat result-list (filter #(<= (:rest-amount %) 0) new-list))))))




;; Problem 32  : 2011/5/18
;; "Elapsed time: 41476.85055 msecs"

;; there is only two type.
;; 1digit * 4digit = 4digit
;; 2digit * 3digit = 4digit

(use 'clojure.contrib.math)

(defn num-list []
  (for [a1 (range 9) a2 (range 8) a3 (range 7) a4 (range 6)
	b1 (range 5) b2 (range 4) b3 (range 3)
	c1 (range 2)]
    (select-nums [a1 a2 a3 a4 b1 b2 b3 c1 0]  [1 2 3 4 5 6 7 8 9])))

(defn select-nums 
  ([col] (select-nums col [1 2 3 4 5 6 7 8 9]))
  ([col digit-list]
     (loop [num-list digit-list
	    coll col
	    res-list []]
       (if (empty? coll)
	 res-list
	 (let [tgt-num (nth num-list (first coll))]
	   (recur (vec (remove #(= % tgt-num) num-list))
		  (rest coll)
		  (conj res-list tgt-num )))))))

(defn list-to-num [digit-list]
  (apply + (map #(* %1 (expt 10 %2)) (reverse digit-list) (iterate inc 0))))


(defn create-nums [col]
  (let [[p pp] (split-at 4 col)
	[mpcand4 mplir1] (map list-to-num (split-at 4 pp))
	[mpcand3 mplir2] (map list-to-num (split-at 3 pp))
	prod (list-to-num p)]
    (if (< prod mpcand4)
      nil
      [[prod mpcand4 mplir1] [prod mpcand3 mplir2]])))


(reduce +
	(distinct (map first
		       (filter #(= (first %) (* (second %) (last %)))
			       (filter (complement empty?)
				       (mapcat create-nums (num-list)))))))
			 


;; jank
(defn get-num-list
  ([col dlist] (get-num-list col dlist ()))
  ([col dlist res-list]
     (if (empty? col)
       res-list
       (let [[num remain] (select-nums (first col) dlist)]
	 (get-num-list (rest col) remain (cons (list-to-num num) res-list))))))

(defn num-list []
  (for [a1 (range 9) a2 (range 8) a3 (range 7) a4 (range 6)
	b1 (range 5) b2 (range 4) b3 (range 3) b4 (range 2)]
    [(get-num-list [[a1 a2 a3 a4] [b1 b2 b3 b4] [0]]  [1 2 3 4 5 6 7 8 9])
     (get-num-list [[a1 a2 a3 a4] [b1 b2 b3] [b4 0]]  [1 2 3 4 5 6 7 8 9])]))




;; Problem 33  : 2011/5/18
;; "Elapsed time: 12.869792 msecs"
;; type
;; [ax]/[ay] = x/y
;;    a=0 or x=y  :: not allowed
;; [xa]/[ya/ = x/y
;;   x=y :: not allowed
;;
;; [ax]/[ya] = x/y
;; [xa]/[ay] = x/y

(defn num-list []
  (for [a (range 9) x (range 8) y (range 7)]
    (select-nums [a x y]  [1 2 3 4 5 6 7 8 9])))

(reduce *
	(map (fn [[a x y]]
	       (let [n (+ (* 10 x) a) m (+ (* 10 a) y)]
		 (if (and (= (/ n m) (/ x y)) (< n m))
		   (/ n m)
		   1))) (num-list)))
	   
	   
(filter (fn [[a x y]] (= (/ (+ (* 10 x) a) (+ (* 10 a) y)) (/ x y))) (num-list))
(filter (fn [[a x y]] (= (/ (+ (* 10 a) x) (+ (* 10 y) a)) (/ x y))) (num-list))


;; Problem 34  : 2011/5/18
;; "Elapsed time: 29005.216026 msecs"

;; 9! * 7 = 2540160 : 7digits
;; 9! * 8 = 2903040 : 7digits 

;; (fact 0) -> 
(def fact (memoize
	   (fn [n] (if (< n 2) 1
		       (* n (fact (dec n)))))))


(filter #(= (first %) (second %))
(map #(list
	(reduce + (map fact (drop-while zero? %)))
	(list-to-num %))
     (for [a1 (range 10)  a2 (range 10)  a3 (range 10)
	   a4 (range 10)  a5 (range 10)  a6 (range 10) a7 (range 10)]
       [a1 a2 a3 a4 a5 a6 a7])))

(filter #(= (first %) (second %))
(map #(list
	(reduce + (map fact (drop-while zero? %)))
	(list-to-num %))
     (for [a1 (range 10)  a2 (range 10)  a3 (range 10)
	   a4 (range 10)  a5 (range 10)  a6 (range 10)]
       [a1 a2 a3 a4 a5 a6])))


(reduce + (map (fn [n]
       (let [x (reduce + (map fact (drop-while zero? n)))
	     y (list-to-num n)]
	 (cond (< x 3) 0
	       (= x y) x
	       :else 0)))
     (for [a1 (range 10)  a2 (range 10)  a3 (range 10)
	   a4 (range 10)  a5 (range 10)  a6 (range 10)]
       [a1 a2 a3 a4 a5 a6])))



;; Problem 35  : 2011/5/19
;; "Elapsed time: 18328.781172 msecs"


(def *prime-list* (atom []))
(create-prime-list-under 1000000)

;;get all ratate num
;; do not work when number includes 0.
(defn get-all-rotate-num [n]
  (if (< n 10) [n]
      (loop [tgt n res-list [n]]
	(let [front (int (/ tgt 10)) tail (rem tgt 10)
	      next (Integer/valueOf (str (str tail) (str front)))]
	  (if (= next n)
	    res-list
	    (recur next (conj res-list next)))))))

;;get all ratate num
(require '[clojure.contrib.string :as ccstr])

(defn rotate-num-str [num-str]
    (str (ccstr/drop (dec (.length num-str)) num-str)
	 (ccstr/butlast 1 num-str) ))

(defn get-all-rotate-num [n]
  (loop [new-num (rotate-num-str (str n))
	 res-list [n]]
      (if (= new-num (str n))
	res-list
	(recur  (rotate-num-str new-num) (conj res-list (Integer/valueOf new-num))))))

(count 
 (map first
      (filter #(every? true? (map is-prime? %))
	      (map get-all-rotate-num @*prime-list*))))

;; Problem 36  : 2011/5/19
;; "Elapsed time: 4715.235037 msecs"

(defn palindromic? [str]
  (= str (ccstr/join "" (reverse str))))

(reduce + (filter #(and (palindromic? (str %))
		     (palindromic? (Integer/toBinaryString %)))
	       (range 1 1000000 2)))



;; Problem 37  : 2011/5/19
;; "Elapsed time: 5385.057877 msecs"
(use 'clojure.contrib.math)

(defn member? [n col]
      (not-every? false? (map #(= n %) col)))

(defn list-to-num [digit-list]
  (apply + (map #(* %1 (expt 10 %2)) (reverse digit-list) (iterate inc 0))))

(defn num-to-list [num]
  (map #(Character/digit % 10) (str num)))

(defn all-prime-left? [numlist]
  (cond (empty? numlist) true
	((complement is-prime?) (list-to-num numlist)) false
	:else (recur (rest numlist))))

(defn all-prime-right? [numlist]
  (cond (empty? numlist) true
	((complement is-prime?) (list-to-num numlist)) false
	:else (recur (butlast numlist))))

(defn prob37? [n]
  (let [numlist (num-to-list n)]
    (cond (< n 10) false
	  (not (member? (first numlist) [2 3 5 7])) false
	  (not (member? (last numlist)  [2 3 7])) false
	  (some even? (rest numlist)) false
	  :else
	  (and (all-prime-left? numlist)
	       (all-prime-right? numlist)))))


(create-prime-list-under 1000000)

(reduce + (filter prob37? @*prime-list*))


;; Problem 38  : 2011/5/29
;; "Elapsed time: 190.967288 msecs"

(defn concat-product [tgt n]
  (apply str (map #(* tgt %) (range 1 (inc n)))))

(defn pandegital-9? [st]
  (= (sort (distinct st)) (seq (str 123456789))))


(first (filter pandegital-9? (map #(concat-product % 2) (range 9999 1000 -1))))


;; Problem 39  : 2011/5/
If p is the perimeter of a right angle triangle with integral length sides, {a,b,c},
there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?

;; a^2 + b^2 = c^2
;;  | a = abs( m^2 - n^2)
;;  | b = 2mn
;;  | c = m^2 + n^2
;;  :  when m>n : p = a+b+c = m^2 + mn = m(m+n)
;;  so serch p
;;  |     p = X * Y (X < Y)
;;  |and  Y - X  < X


(defn count-pitagorean-triplet [num]
  (count
  (filter #(not (= % nil))
      (for [a (range 1 (inc (ceil (/ num 3))))
            b (range (inc a) (inc (ceil (/ (- num a) 2))))]
        (let [c (- num (+ a b))]
          (if (= (+ (expt a 2) (expt b 2))
                 (expt c 2))
              (list (* a b c) [a b c])))))))

(defn pitagorean-triplet [num]
  (filter #(not (= % nil))
      (for [a (range 1 (inc (ceil (/ num 3))))
            b (range (inc a) (inc (ceil (/ (- num a) 2))))]
        (let [c (- num (+ a b))]
          (if (= (+ (expt a 2) (expt b 2))
                 (expt c 2))
	    (list (* a b c) [a b c]))))))

user> (time (doall (pmap #(pitagorean-triplet %) (range 1000))))
"Elapsed time: 103524.627012 msecs"

(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 3 0 0 0 0 0 1 0 0 0 0 0 2 0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 1 0 0 0 3 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 1 0 0 0 1 0 0 0 1 0 2 0 0 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 1 0 0 0 3 0 0 0 0 0 1 0 2 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 1 0 1 0 0 0 2 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 2 0 0 0 0 0 3 0 0 0 1 0 0 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 0 0 4 0 0 0 1 0 0 0 0 0 0 0 1 0 1 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 2 0 1 0 0 0 3 0 0 0 1 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 1 0 1 0 5 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 2 0 1 0 1 0 0 0 1 0 2 0 0 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 0 0 1 0 0 0 4 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 2 0 0 0 0 0 1 0 0 0 2 0 0 0 0 0 0 0 3 0 0 0 1 0 0 0 0 0 0 0 3 0 0 0 1 0 2 0 0 0 0 0 2 0 0 0 0 0 0 0 3 0 0 0 1 0 0 0 0 0 2 0 1 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 1 0 0 0 1 0 3 0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 3 0 0 0 0 0 4 0 0 0 0 0 1 0 0 0 1 0 0 0 1 0 1 0 1 0 1 0 0 0 0 0 0 0 0 0 5 0 0 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 1 0 0 0 2 0 1 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 6 0 0 0 0 0 0 0 2 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 0 0 4 0 0 0 2 0 0 0 0 0 0 0 1 0 2 0 0 0 0 0 0 0 0 0 4 0 1 0 1 0 0 0 0 0 0 0 3 0 0 0 0 0 1 0 2 0 0 0 1 0 0 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 1 0 0 0 1 0 0 0 8 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1 0 0 0 0 0 3 0 0 0 0 0 2 0 0 0 1 0 1 0 0 0 3 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 4 0 0 0 0 0 0 0 0 0 2 0 2 0 0 0 0 0 2 0 2 0 0 0 5 0 0 0 1 0 1 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 2 0 0 0 0 0 0 0 4 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 1 0 1 0 1 0 4 0 1 0 0 0 1 0 0 0)


(def count-list '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 3 0 0 0 0 0 1 0 0 0 0 0 2 0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 1 0 0 0 3 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 1 0 0 0 1 0 0 0 1 0 2 0 0 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 1 0 0 0 3 0 0 0 0 0 1 0 2 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 1 0 1 0 0 0 2 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 2 0 0 0 0 0 3 0 0 0 1 0 0 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 0 0 4 0 0 0 1 0 0 0 0 0 0 0 1 0 1 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 2 0 1 0 0 0 3 0 0 0 1 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 1 0 1 0 5 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 2 0 1 0 1 0 0 0 1 0 2 0 0 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 0 0 1 0 0 0 4 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 2 0 0 0 0 0 1 0 0 0 2 0 0 0 0 0 0 0 3 0 0 0 1 0 0 0 0 0 0 0 3 0 0 0 1 0 2 0 0 0 0 0 2 0 0 0 0 0 0 0 3 0 0 0 1 0 0 0 0 0 2 0 1 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 1 0 0 0 1 0 3 0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 3 0 0 0 0 0 4 0 0 0 0 0 1 0 0 0 1 0 0 0 1 0 1 0 1 0 1 0 0 0 0 0 0 0 0 0 5 0 0 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 1 0 0 0 2 0 1 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 6 0 0 0 0 0 0 0 2 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 0 0 4 0 0 0 2 0 0 0 0 0 0 0 1 0 2 0 0 0 0 0 0 0 0 0 4 0 1 0 1 0 0 0 0 0 0 0 3 0 0 0 0 0 1 0 2 0 0 0 1 0 0 0 0 0 2 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 1 0 0 0 1 0 0 0 8 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1 0 0 0 0 0 3 0 0 0 0 0 2 0 0 0 1 0 1 0 0 0 3 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 4 0 0 0 0 0 0 0 0 0 2 0 2 0 0 0 0 0 2 0 2 0 0 0 5 0 0 0 1 0 1 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 2 0 0 0 0 0 0 0 4 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 1 0 1 0 1 0 4 0 1 0 0 0 1 0 0 0))

(map list count-list (iterate inc 1))



(let [tgt 998] (factors (/ tgt 2)))

(defn paird-divisor [n]
  (filter #(not= 0 %)
	  (map #(if (zero? (rem n %))
		  [% (/ n %)]
		  0)
	       (range  (int (ceil (sqrt n))) (inc n)))))

(map (fn [x]
       (take-while (fn [[Y X]] (and (< (- Y X) X) (not= X Y)))
		   (paird-divisor x)))
     (range 500 1 -1))


(let [x 30]
(take-while (fn [[Y X]] (and (< (- Y X) X) (not= X Y)))
		   (paird-divisor x)))

(gcd 375 200)
(/ 375 25)
(/ 425 25)


;; Problem 40  : 2011/5/
;; "Elapsed time: 6926.687051 msecs"
(use 'clojure.contrib.math)

(defn fractional-part-of-irrational []
  (mapcat #(seq (str %)) (iterate inc 1)))

(filter (fn [[tgt num]]
	  (= (map #(expt 10 %) (range 3)) num))
	(map list
	     (fractional-part-of-irrational)
	     (iterate inc 1)))

(loop [seq (map list (fractional-part-of-irrational) (iterate inc 1))
       tgt (map #(expt 10 %) (range 7))
       res 1]
  (if (empty? tgt)
    res
    (let [[digit num] (first seq)]
      (if (= num (first tgt))
	(recur (rest seq) (rest tgt) (* (Character/digit digit 10) res))
	(recur (rest seq) tgt res)))))
	

