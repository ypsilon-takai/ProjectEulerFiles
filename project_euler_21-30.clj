;;;
;;;  Project euler  21-30
;;;             Author : Yosi
;;;             From   : 2011/5/4
;;;

;; Problem 21  : 2011/5/5
;;"Elapsed time: 1260.253748 msecs"
;; prime list
(def *prime-list* (atom []))

(defn is-prime? [target]
  (loop [plist @*prime-list*]
    (let [check-num (first plist)]
      (cond (empty? plist) true
            (> check-num (Math/sqrt target)) true
            (zero? (rem target check-num)) false
            true (recur (rest plist))))))

(defn create-prime-list-under [n]
  (loop [target 2]
    (if (>= target n)
      true
      (if (is-prime? target)
        (do
          (reset! *prime-list* (conj @*prime-list* target))
          (recur (inc target)))
        (recur (inc target))))))

(create-prime-list-under 100000)

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

(defn multi-plus [lst]
  ;; multi-plus : (2 2 2) -> (+ 8 4 2 1)
  (if (empty? lst)
    1
    (+ (apply * lst) (multi-plus (rest lst)))))

(defn sum-of-divisor [n]
  (let [ ftr (factors n)]
    (- (reduce *
            (for [tgt (distinct ftr)]
              (multi-plus (filter #(= % tgt) ftr))))
       n)))


(reduce +
	(filter (fn [x] (let [y (sum-of-divisor x)
			      z (sum-of-divisor y)]
			  (and (= x z) (not (= x y)))))
		(range 2 10000)))



;; Problem 22  : 2011/5/9
;; "Elapsed time: 49.662736 msecs"

(use '[clojure.contrib.duck-streams :only (reader read-lines)])

(def char-val '{
\A 1 \B 2 \C 3 \D 4 \E 5
\F 6 \G 7 \H 8 \I 9 \J 10
\K 11 \L 12 \M 13 \N 14 \O 15
\P 16 \Q 17 \R 18 \S 19 \T 20
\U 21 \V 22 \W 23 \X 24 \Y 25
\Z 26})


(defn calc-words [list]
  (let [[word num] list]
    (* (reduce + (map char-val word)) num)))


(let [file-data (with-open [] (read-lines "D:/Profiles/q3197c/Desktop/names.txt"))
      name-list (.split (.replaceAll (first file-data) "\"" "") ",")]
  (reduce +
	  (map calc-words
	       (map list  (sort name-list) (iterate inc 1)))))



;; Problem 23  : 2011/5/10
;;  step1 : "Elapsed time: 7139855.954877 msecs" (ouch!)

(def *prime-list* (atom []))

(defn is-prime? [target]
  (loop [plist @*prime-list*]
    (let [check-num (first plist)]
      (cond (empty? plist) true
            (> check-num (Math/sqrt target)) true
            (zero? (rem target check-num)) false
            true (recur (rest plist))))))

(defn create-prime-list-under [n]
  (loop [target 2]
    (if (>= target n)
      true
      (if (is-prime? target)
        (do
          (reset! *prime-list* (conj @*prime-list* target))
          (recur (inc target)))
        (recur (inc target))))))

(create-prime-list-under 100000)

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

(defn multi-plus [lst]
  ;; multi-plus : (2 2 2) -> (+ 8 4 2 1)
  (if (empty? lst)
    1
    (+ (apply * lst) (multi-plus (rest lst)))))

(defn sum-of-divisor [n]
  (let [ ftr (factors n)]
    (- (reduce *
            (for [tgt (distinct ftr)]
              (multi-plus (filter #(= % tgt) ftr))))
       n)))

(def *all-abandant-num*
     (filter #(> (sum-of-divisor %) %) (range 2 28123)))


;; bruto force!
(defn member [n col]
  (cond (empty? col) false
	(= n (first col)) true
	:else (recur n (rest col))))

(defn sum-of-abandant-num? [n]
  (loop [all-abandant-num *all-abandant-num*]
    (cond (empty? all-abandant-num) false
	  (> (first all-abandant-num) n) false
	  (member (- n (first all-abandant-num)) all-abandant-num) true
	  :else (recur (rest all-abandant-num)))))

(reduce + (filter #(not (sum-of-abandant-num? %)) (range 1 28123)))




;; is this better?
(def soa-map (atom {}))

(loop [all-abandant-num *all-abandant-num*]
  (let [one (first all-abandant-num)]
    (do
      (doseq (for [tgt all-abandant-num]
	       (swap! soa-set conj (+ one tgt))))
      (recur (rest all-abandant-num)))))


(loop [all-abandant-num *all-abandant-num*]
  (let [one (first all-abandant-num)]
    (do
      (for [tgt all-abandant-num]
	(if (< (+ one tgt) 28123)
	  (swap! soa-map assoc (+ one tgt) 1)))
      (recur (rest all-abandant-num)))))


;; Problem 24  : 2011/5/10
;; "Elapsed time: 1.543492 msecs"  
(def fact-tree
     [362880 40320 5040 720 120 24 6 2 1])

(use 'clojure.contrib.math)

(loop [fact-tree fact-tree
       num-list [0 1 2 3 4 5 6 7 8 9]
       remains (dec 1000000)
       res-list ()]
  (if (empty? fact-tree)
    (concat (reverse res-list) num-list)
    (let [div (floor (/ remains (first fact-tree)))
	  tgt-num (nth num-list div)]
      (recur (rest fact-tree)
	     (remove #(= % tgt-num) num-list)
	     (rem remains (first fact-tree))
	     (cons tgt-num res-list)))))


;; Problem 25  : 2011/5/10
;; "Elapsed time: 0.373232 msecs"
(defn my-fibo
  ([]
     (concat [1 1] (my-fibo 1 1)))
  ([x y]
     (let [next-num (+ x y)]
       (lazy-seq
        (cons next-num (my-fibo y next-num))))))

(take 1 (drop-while #(< (first %) (expt 10 999)) (map list (my-fibo) (iterate inc 1))))


;; Problem 26  : 2011/5/11
;; "Elapsed time: 63.869011 msecs"
(def *prime-list* (atom []))
(create-prime-list-under 1000)


(loop [length-prime-list (for [n (take 10 (reverse @*prime-list*))]
			   (list (first (filter #(= 1 (rem (expt 10 %) n)) (range 1 n)))
				 n))
       old-len 0
       old-prim 0]
  (let [[length prime] (first length-prime-list)]
    (cond (> old-len prime) [old-len old-prim]
	  (< old-len length) (recur (rest length-prime-list) length prime)
	  :else (recur (rest length-prime-list) old-len old-prim))))

    
;; Problem 27  : 2011/5/12
;;"Elapsed time: 0.776914 msecs"

(use 'clojure.contrib.math)

(for [b (take-while #(< % 1000) @*prime-list*)
      a (range -999 999 2)]
   (count (take-while is-prime? (map #(+ (expt % 2) (* a %) b) (range))))))
  
(map #(+ (expt % 2) (* -79 %) 1601) (range 80))
(1601 1523 1447 1373 1301 1231 1163 1097 1033 971 911 853 797 743 691 641 593 547 503 461 421 383 347 313 281 251 223 197 173 151 131 113 97 83 71 61 53 47 43 41 41 43 47 53 61 71 83 97 113 131 151 173 197 223 251 281 313 347 383 421 461 503 547 593 641 691 743 797 853 911 971 1033 1097 1163 1231 1301 1373 1447 1523 1601)


(map #(+ (expt % 2) % 41) (range 40))
(map #(+ (expt % 2) (* -79 %) 1601) (range 80))

(41 43 47 53 61 71 83 97 113 131 151 173 197 223 251 281 313 347 383 421 461 503 547 593 641 691 743 797 853 911 971 1033 1097 1163 1231 1301 1373 1447 1523 1601)

(take 30 (map #(apply - %) (partition 2 1 (map #(apply - %) (partition 2 1 @*prime-list*)))))

(take 100 (map list (map #(apply - %) (partition 2 1 (map #(apply - %) (partition 2 1 @*prime-list*)))) (partition 3 1 @*prime-list*)))

(map #(apply - % ) (partition 2 1 [41 43 47 53 61 71 83 97 113 131 151 173 197 223 251 281 313 347 383 421 461 503 547 593 641 691 743 797 853 911 971 1033 1097 1163 1231 1301 1373 1447 1523 1601]))

(defn inc-list [start-val inc-val]
  (lazy-seq
   (cons start-val
	 (inc-list (+ start-val inc-val)
		   (+ inc-val 2)))))

(take-while is-prime? (take 10 (inc-list 41 2)))

(for [prime-num (take-while #(< % 1000) @*prime-list*)]
  (let [dat (take-while is-prime? (inc-list prime-num 2))]
    [prime-num (count dat)]))


(map #(apply - %) (partition 2 1 (map #(apply - %) (partition 2 1 
'(41 43 47 53 61 71 83 97 113 131 151 173 197 223 251 281 313 347 383 421 461 503 547 593 641 691 743 797 853 911 971 1033 1097 1163 1231 1301 1373 1447 1523 1601)))))

(map #(apply - %) (partition 2 1 '(41 43 47 53 61 71 83 97 113 131 151 173 197 223 251 281 313 347 383 421 461 503 547 593 641 691 743 797 853 911 971 1033 1097 1163 1231 1301 1373 1447 1523 1601)))


(def num-list (reverse (map #(+ (expt % 2) % 41) (range 40))))

(filter #(integer? (second %)) (map #(list % (sqrt (- (* 4 %) 163))) (drop-while #(> 999) num-list)))

(take 10 (map #(+ (expt % 2) (* -61 %) 971) (range)))



;; Problem 28  : 2011/5/13
;; "Elapsed time: 1.278934 msecs"

1 9   25  49  81  121
  8 16  24  32  40
   8  8   8   8

1 3 13 31 57 91
 2 10 18 26 34
  8  8  8  8)

1 5 17 37 65 101
 4 12 20 28 36
  8  8  8  8)

1 7 21 43 73 111
 6 14 22 30 38
  8  8  8  8)

(defn get-diffs [col]
  (map #(- (apply - %)) (partition 2 1 col)))

(defn sq-corner
  ([] (cons 1 (sq-corner 1)))
  ([num]
     (lazy-seq
      (concat
       (list
	(+ (* 4 (expt num 2)) (* -2 num) 1)
	(+ (* 4 (expt num 2))  1)
	(+ (* 4 (expt num 2)) (* 2 num) 1)
	(+ (* 4 (expt num 2)) (* 4 num) 1))
       (sq-corner (inc num))))))


(map #(+ (* 4 (expt % 2)) (* -2 %) 1) (range 10))
(map #(+ (* 4 (expt % 2)) 1) (range 10))
(map #(+ (* 4 (expt % 2)) (* 2 %) 1) (range 10))
(map #(+ (* 4 (expt % 2)) (* 4 %) 1) (range 10))


(+ 1 (reduce + (map #(+ (+ (* 4 (expt % 2)) (* -2 %) 1)
		   (+ (* 4 (expt % 2)) 1)
		   (+ (* 4 (expt % 2)) (* 2 %) 1)
		   (+ (* 4 (expt % 2)) (* 4 %) 1)) (range 1 3))))

(defn prob28 [n]
  (let [ply (/ (+ n 1) 2)]
    (+ 1
       (reduce +
	       (map #(+ (* 16 (expt % 2)) (* 4 %) 4)
		    (range 1 ply))))))


;; Problem 29  : 2011/5/13
;; "Elapsed time: 98.855302 msecs"

(count (distinct (for [a (range 2 101) b (range 2 101)] (expt a b))))


;; Problem 30  : 2011/5/
;; "Elapsed time: 24452.933823 msecs"

(reduce #(+ %1 (first %2)) 0
 (drop 2
   (filter #(= (first %) (second %))
	   (for [a1 (range 10)  a2 (range 10)  a3 (range 10)
		 a4 (range 10)  a5 (range 10)  a6 (range 10)]
	     [(+ (expt a1 5) (expt a2 5) (expt a3 5) (expt a4 5) (expt a5 5) (expt a6 5))
	      (+ (* a1 (expt 10 5)) (* a2 (expt 10 4)) (* a3 (expt 10 3))
		 (* a4 (expt 10 2)) (* a5 (expt 10 1)) (* a6 (expt 10 0)))]))))



(reduce #(+ %1 (first %2)) 0
 (drop 2
   (filter #(= (first %) (second %))
	   (for [a1 (range 10)  a2 (range 10)  a3 (range 10)
		 a4 (range 10)  a5 (range 10)  a6 (range 10)]
	     [(+ (expt a1 4) (expt a2 4) (expt a3 4) (expt a4 4) (expt a5 4) (expt a6 4))
	      (+ (* a1 (expt 10 5)) (* a2 (expt 10 4)) (* a3 (expt 10 3))
		 (* a4 (expt 10 2)) (* a5 (expt 10 1)) (* a6 (expt 10 0)))]))))



