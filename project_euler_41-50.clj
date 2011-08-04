;;;
;;;  Project euler  41-50
;;;             Author : Yosi
;;;             From   : 2011/6/9
;;;


;; Problem 41  : 2011/6/9
;;"Elapsed time: 28480.106271 msecs"
;;"Elapsed time: 2567.498384 msecs"
;;"Elapsed time: 5.947124 msecs"

(use 'clojure.contrib.math)

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

(defn num-list9 []
  (for [a1 (range 9) a2 (range 8) a3 (range 7) a4 (range 6)
	a5 (range 5) a6 (range 4) a7 (range 3) a8 (range 2)]
    (select-nums [a1 a2 a3 a4 a5 a6 a7 a8 0]  [9 8 7 6 5 4 3 2 1])))

(defn num-list8 []
  (for [a2 (range 8) a3 (range 7) a4 (range 6)
	a5 (range 5) a6 (range 4) a7 (range 3) a8 (range 2)]
    (select-nums [a2 a3 a4 a5 a6 a7 a8 0]  [8 7 6 5 4 3 2 1])))

(defn num-list7 []
  (for [a3 (range 7) a4 (range 6)
	a5 (range 5) a6 (range 4) a7 (range 3) a8 (range 2)]
    (select-nums [a3 a4 a5 a6 a7 a8 0]  [7 6 5 4 3 2 1])))


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


(time (doall (take 1 (filter is-prime? (map list-to-num (num-list9))))))
;;"Elapsed time: 28480.106271 msecs"
(time (doall (take 1 (filter is-prime? (map list-to-num (num-list8))))))
;;"Elapsed time: 2567.498384 msecs"
(time (doall (take 1 (filter is-prime? (map list-to-num (num-list7))))))
;;"Elapsed time: 5.947124 msecs"


;;
(use 'clojure.contrib.combinatorics)

(take 1
      (flatten 
       (for [n [7 4]]
	 (filter is-prime? (map list-to-num (permutations (range n 0 -1)))))))

"Elapsed time: 5.017956 msecs"



;; Problem 42  : 2011/6/9
"Elapsed time: 22.138288 msecs"

(use '[clojure.contrib.duck-streams :only (reader read-lines)])

(def alpha-pos
     '{\A 1 \B 2 \C 3 \D 4 \E 5 \F 6 \G 7 \H 8 \I 9 \J 10
       \K 11 \L 12 \M 13 \N 14 \O 15 \P 16 \Q 17 \R 18 \S 19 \T 20
       \U 21 \V 22 \W 23 \X 24 \Y 25 \Z 26})

(defn calc-words [st]
  (reduce + (map #(alpha-pos %) st)))

(defn triangle-num? [n]
  (let [double-square-int (int (sqrt (* n 2)))
        num-1 double-square-int
        num-2 (inc num-1)]
    (= (/ (* num-1 num-2) 2) n)))


(count (let [file-data (with-open [] (read-lines "D:/Profiles/q3197c/Desktop/pe42_words.txt"))
      word-list (.split (.replaceAll (first file-data) "\"" "") ",")]
	 (filter triangle-num? (map calc-words word-list))))



;; Problem 43  : 2011/6/10
;; "Elapsed time: 724.777311 msecs"

;; d2d3d4=406 is divisible by 2  => d4 is even.
;; d3d4d5=063 is divisible by 3  => d3+d4+d5 is divisible by 3
;; d4d5d6=635 is divisible by 5  => d6 is 0 or 5
;; d5d6d7=357 is divisible by 7
;; d6d7d8=572 is divisible by 11 => !
;; d7d8d9=728 is divisible by 13 
;; d8d9d10=289 is divisible by 17
;;
;; if d6 == 0 := d7 and d8 should same. no way.
;; d6 = 5
;; d6d7d8 -> [5]d7d8 is a multiple of 11
;;   ->   506 517 528 539 561 572 583 594  (550 is not match the criteria)
;;
;; d678 [5 0 6] [5 1 7] [5 2 8] [5 3 9] [5 6 1] [5 7 2] [5 8 3] [5 9 4]
;; d4 [0 2 4 6 8]

(use 'clojure.set)
(use 'clojure.contrib.math)

(defn list-diff [base col]
    (vec (difference (set base) (set col))))

(defn create-digits [[d6 d7 d8 d4 :as col]]
  (let [rest-list (list-diff (range 10) col)]
    (for [d1 (range 6) d2 (range 5) d3 (range 4) d5 (range 3) d9 (range 2) ]
      (let [[d1 d2 d3 d5 d9 d10] (select-nums [d1 d2 d3 d5 d9 0] rest-list)]
	[d1 d2 d3 d4 d5 d6 d7 d8 d9 d10]))))

(defn list-to-num [digit-list]
  (apply + (map #(* %1 (expt 10 %2)) (reverse digit-list) (iterate inc 0))))


(defn filter-43 [[d1 d2 d3 d4 d5 d6 d7 d8 d9 d10]]
  (and (zero? (rem (+ d3 d4 d5) 3))
       (zero? (rem (list-to-num [d5 d6 d7]) 7))
       (zero? (rem (list-to-num [d7 d8 d9]) 13))
       (zero? (rem (list-to-num [d8 d9 d10]) 17))))

(let [d6784 (for [d678 [[5 0 6] [5 1 7] [5 2 8] [5 3 9] [5 6 1] [5 7 2] [5 8 3] [5 9 4]]
		  d4 [0 2 4 6 8]
		  :when (not-any? #(= % d4) d678)]
	      (conj d678 d4))]
  (reduce + (map list-to-num (filter filter-43 (mapcat create-digits  d6784)))))

(1430952867 4130952867 1460357289 4160357289 1406357289 4106357289)
  



;; Problem 44  : 2011/6/13
;;"Elapsed time: 10744.530939 msecs"
;;
;; Memoized!!??
;; "Elapsed time: 38270.006104 msecs"	   
(let [n 7] (/ (* n (- (* 3 n) 1)) 2))

(let [n 51] (sqrt (+ (* 24 n) 1)))

(let [n 51] (/ (+ (sqrt (+ (* 24 n) 1)) 1) 6))

(defn square? [n]
  (= (sqrt n) (int (sqrt n))))

(use 'clojure.contrib.math)

(defn pentagonal [n]
  (/ (* n (- (* 3 n) 1)) 2))

(def pentagonal (memoize pentagonal))

#ouch!
(defn pentagonal? [n]
  (square? (+ (* 24 n) 1)))

(defn pentagonal? [n]
  (zero? (rem (+ 1 (sqrt (+ 1 (* 24 n)))) 6)))

(def pentagonal? (memoize pentagonal?))

(defn sum-list [n]
  (map #(list % (- n %)) (range 1 (/ n 2))))


(let [n 11
      key-val (pentagonal n)]
  (for [[n-ch1 n-ch2] (sum-list n)]
    [[(pentagonal n-ch1) (pentagonal n-ch2) (pentagonal? (- key-val (* 3 n-ch1 n-ch2)))]
     key-val]))

(let [n 11
      key-val (pentagonal n)]
  (for [[n-ch1 n-ch2] (sum-list n)]
    (let [p-ch1(pentagonal n-ch1) p-ch2 (pentagonal n-ch2)]
      [(and (pentagonal? (+ p-ch2 p-ch1))
	    (pentagonal? (- p-ch2 p-ch1)))
       p-ch1
       p-ch2])))

(take 2
      (filter #(not (empty %))
      (for [n (iterate inc 5)]  
	(let [key-val (pentagonal n)]
	  (filter #(true? (first %))
		  (for [[n-ch1 n-ch2] (sum-list n)]
		    (let [p-ch1(pentagonal n-ch1) p-ch2 (pentagonal n-ch2)]
		      [(and (pentagonal? (+ p-ch2 p-ch1))
			    (pentagonal? (- p-ch2 p-ch1)))
		       p-ch1
		       p-ch2])))))))

(take 1
(for [num (iterate inc 5)]
  (let [tgt (pentagonal num)]
    (for [child-num (range 1 num)]
      (let [child (pentagonal child-num)]
	[(and (pentagonal? (+ tgt child))
	      (pentagonal? (- tgt child)))
	 tgt
	 child])))))

;; final
(take 1
(filter #(true? (first %))
	(mapcat (fn [num] 
		  (let [tgt (pentagonal num)]
		    (for [child-num (range (dec num) 0 -1)]
		      (let [child (pentagonal child-num)]
			[(and (pentagonal? (+ tgt child))
			      (pentagonal? (- tgt child)))
			 tgt
			 child]))))
		(iterate inc 5))))



;; Problem 45  : 2011/6/13
;; "Elapsed time: 248.191879 msecs"

(defn triangle-num [n]
  (/ (* n (+ 1 n)) 2))

(defn triangle-num? [n]
  (zero? (rem (- (sqrt (+ (* 8 n) 1)) 1) 2)))

(defn pentagonal [n]
  (/ (* n (- (* 3 n) 1)) 2))

(defn pentagonal? [n]
  (zero? (rem (+ 1 (sqrt (+ 1 (* 24 n)))) 6)))

(defn hexagonal [n]
  (* n (- (* 2 n) 1)))

(defn hexagonal? [n]
  (zero? (rem (+ 1 (sqrt (+ 1 (* 8 n)))) 4)))


(take 3
(filter (fn [x]
	  (and (triangle-num? n)
	       (pentagonal? n)))
	(map hexagonal (iterate inc 1))))


;; Problem 46  : 2011/6/13
;; "Elapsed time: 841.523433 msecs"

(defn square? [n]
  (= (sqrt n) (int (sqrt n))))

(defn not-prime-plus-double-sq [n]
    (every? false?
	  (map (fn [pnum]
		 (let [tstval (- n pnum)]
		    (and (even? tstval)
			 (square? (/ tstval 2 )))))
	       (prime-nums-under n))))

(take 1 (filter not-prime-plus-double-sq
		(filter (complement is-prime?) (range 1 1000000 2))))



;; Problem 47  : 2011/6/13
;; "Elapsed time: 162902.866705 msecs"
;;factors

(take 1
      (filter
       (fn [coll] (every? #(= 4 (first %)) coll))
       (partition 4 1
		  (map #(vector (count (distinct (factors %))) %)
		       (range 1 1000000)))))



;; Problem 48  : 2011/6/13
;; "Elapsed time: 217.941386 msecs"

(reduce + (map #(rem (expt % %) 10000000000)(range 1 1001)))



;; Problem 49  : 2011/6/14
;; "Elapsed time: 182.153877 msecs"

(drop-while #(<= % 1000) (prime-nums-under 10000))
;; count : 1061

(use 'clojure.contrib.math)

(defn permutation?  [[m n o]]
     (= (sort (num-to-list n))
	(sort (num-to-list m))
	(sort (num-to-list o))))

(defn sequential? [n]
  (let [[x y z] (sort n)]
    (= (- y x)
       (- z y))))



(loop [p-list (drop-while #(<= % 1000) (prime-nums-under 10000)) reslist []]
  (if (< (count p-list) 3)
    reslist
    (let [tgt (first p-list)
	  rest (rest p-list)]
      (filter #(is-prime? (- (* 2 %) tgt)) (take-while #(< (- (* 2 %) tgt) 10000) rest)))))



(loop [p-list (drop-while #(<= % 1000) (prime-nums-under 10000))
       reslist []]
  (if (< (count p-list) 3)
    reslist
    (recur (rest p-list)
	   (let [tgt (first p-list)
		 rest (rest p-list)]
	      (concat
	       reslist
	       (take-while #(and (< (- (* 2 %) tgt) 10000)
				 (= (sort (num-to-list %))
				    (sort (num-to-list tgt))))
			   rest))))))




;;;; another

(defn seq-of-3 [coll]
  (for [a coll b coll c coll :when (= (- (* b 2) a) c) :when (< a b) :when (< b c)]
    [a b c]))


(let [four-digit-primes (drop-while #(< % 1000) (prime-nums-under 10000))
      *digits-map* (atom {})]

(dorun
  (map (fn [n]
	 (let [key (sort (num-to-list n))]
	   (swap! *digits-map* assoc key (cons n (@*digits-map* key)))))
       four-digit-primes))

(mapcat seq-of-3
	(for [key (keys @*digits-map*) :when (>= (count (@*digits-map* key)) 3)]
	  (sort (@*digits-map* key))))
)





;; Problem 50  : 2011/6/14
;; "Elapsed time: 276190.732342 msecs"

(defn longest-seq [coll max-val]
  (loop [forward-coll []
	 rest-coll coll
	 max-seq []]
    (if (empty? rest-coll)
      max-seq
      (let [next-foward (conj forward-coll (first rest-coll))
	    sum-of-coll (reduce + next-foward)]
	(cond (> sum-of-coll max-val) max-seq
	      (is-prime? sum-of-coll)
	         (recur next-foward (rest rest-coll) next-foward)
	      :else
		 (recur next-foward (rest rest-coll) max-seq ))))))


(loop [primes (prime-nums-under 1000000)
       max-count 0
       res-seq []]
  (if (< (count primes) max-count)
    res-seq
    (let [new-long-coll (longest-seq primes 1000000)
	  new-count (count new-long-coll)]
      (if (< new-count max-count)
	(recur (rest primes) max-count res-seq)
	(recur (rest primes) new-count new-long-coll)))))




