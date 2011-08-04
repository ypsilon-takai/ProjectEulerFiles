;;;
;;;  Project euler
;;;             Author : Yosi
;;;             From   : 2011/4/4
;;;

;; Problem 1  : 2011/4/4
;;   "Elapsed time: 8.390731 msecs"
(defn is-3x-or-5x? [x]
  (or (zero? (rem x 3))
      (zero? (rem x 5))))

(reduce + (filter is-3x-or-5x? (range 1 10000)))


;; Problem 2  2011/4/4
;;   "Elapsed time: 0.794515 msecs"
(defn my-fibo
  ([]
     (concat [1 2] (my-fibo 1 2)))
  ([x y]
     (let [next-num (+ x y)]
       (lazy-seq
        (cons next-num (my-fibo y next-num))))))

(reduce + (filter even? (take-while #(< % 4000000) (my-fibo))))


;; Problem 3  : 2011/4/6
(use 'clojure.contrib.math)

;; overflow with 10000
(defn prime-nums-under [n]
  (let [limt (inc (int (sqrt n)))]
    (loop [prm-nums () candidate (range 2 (inc n))]
      (let [target (first candidate)]
    (if (>= target limt)
      (concat prm-nums candidate)
      (recur (concat prm-nums (list target))
         (filter #(not (zero? (rem % target))) (rest candidate))))))))


;; java.lang.OutOfMemoryError: Java heap space
(loop [tester 600851475143
       target-prim (prime-nums-under (floor(sqrt tester)))
       successor 1]
  (cond (empty? target-prim) successor
    (zero? (rem tester (first target-prim))) (recur tester (drop 1 target-prim) (first target-prim))
    true (recur tester (drop 1 target-prim) successor)))

;; SUCCESS
(defn is-prime? [n]
  (zero? (count (filter #(= (rem n %) 0) (range 2 (inc (int (sqrt n))))))))

(loop [tester 600851475143
       target-num (floor (sqrt tester))]
  (if (and (zero? (rem tester target-num))
	   (is-prime? target-num))
    target-num
    (recur tester (- target-num 1))))

;; trace
;; (use 'clojure.contrib.trace)


;; Problem 4  : 2011/4/7
(* 999 999)     ;998001
(* 100 100)     ;10000

(use 'clojure.contrib.math)

(defn palindromic-5-digit-num []
  (for [one (range 9 1 -1) two (range 9 0 -1) three (range 9 0 -1)]
    (sort (+ (* 10000 one) (* 1000 two) (* 100 three) (* 10 two) one))))

(defn palindromic-6-digit-num []
  (for [one (range 9 0 -1) two (range 9 -1 -1) three (range 9 -1 -1)]
    (+ (* 100000 one) (* 10000 two) (* 1000 three) (* 100 three) (* 10 two) one)))

(defn is-multiple-of-three-digits? [n]
  (loop [tester (range 100 1000)]
    (let [target (first tester)]
      (cond (empty? tester) false
        (and (zero? (rem n target))
             (>= (/ n target) 100 )
             (< (/ n target) 1000))  (list target (/ n target))
         true (recur (rest tester))))))


(loop [digit-list (palindromic-6-digit-num)]
  (let [ans (is-multiple-of-three-digits? (first digit-list))]
    (if (seq? ans)
      [(first digit-list) ans]
      (recur (rest digit-list)))))

(loop [digit-list (palindromic-5-digit-num)]
  (let [ans (is-multiple-of-three-digits? (first digit-list))]
    (if (seq? ans)
      [(first digit-list) ans]
      (recur (rest digit-list)))))


;; Problem 5  : 2011/4/7
;; prime-nums-under : defined above

(defn factors [n]
  ;; (foctors 12) => (2 2 3)
  (loop [factor-list ()
     target n
     prime-list (prime-nums-under target)]
    (if (or (= target 1) (empty? prime-list))
      (reverse factor-list)
      (let [one-prime (first prime-list)]
    (if (zero? (rem target one-prime))
      (recur (cons one-prime factor-list) (/ target one-prime) prime-list)
      (recur factor-list target (rest prime-list)))))))


(defn fusion
  ;; (fusion '(2 3 3) '(3 4 4)) => (2 3 3 4 4)
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

(reduce * (reduce fusion (map #(factors %) (range 2 21))))


;; Problem 6  : 2011/4/7
;;   "Elapsed time: 0.627733 msecs"
(- (expt (reduce + (range 1 101)) 2)
   (reduce + (map #(expt % 2) (range 1 101))))



;; Problem 7  : 2011/4/8
;;   "Elapsed time: 100831.996245 msecs"
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

(drop 10000 (find-prime 10001))


;; Problem 8  : 2011/4/8
;;   "Elapsed time: 114.020916 msecs"
(def target1000num 
"73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450")

(defn char-to-num [ch]
  (Character/digit ch 10))

(def target-num-list (map #(char-to-num %)
              (filter #(not (= % \newline)) target1000num)))

(loop [tgt-num-list (rest target-num-list)
       result-list (list (reduce * (take 5 target-num-list))
             (take 5 target-num-list))]
  (if (> 5 (count tgt-num-list))
    result-list
    (let [next-5 (take 5 tgt-num-list)
      product-of-5 (reduce * next-5)]
      (if (< product-of-5 (first result-list))
    (recur (drop 1 tgt-num-list) result-list)
    (recur (drop 1 tgt-num-list) (list product-of-5 next-5))))))
      

;; Problem 9  : 2011/4/8
;;  "Elapsed time: 0.943137 msecs"
;; a^2 + b^2 = c^2
;; a + b + c = 1000

(use 'clojure.contrib.math)

(defn third+1 [n]
  (if (zero? (rem n 3))
    (inc (/ n 3))
    (inc (int (/ n 3)))))

(defn half+1 [n]
  (if (zero? (rem n 2))
    (inc (/ n 2))
    (inc (int (/ n 2)))))

(let [max-num 1000]
  (filter #(not (= % nil))
      (for [a (range 1 (third+1 max-num))
            b (range (inc a) (half+1 (- max-num a)))]
        (let [c (- max-num (+ a b))]
          (if (= (+ (expt a 2) (expt b 2))
                 (expt c 2))
              (list (* a b c) [a b c]))))))


;; Problem 10  : 2011/4/9
;;  "Elapsed time: 13662.498447 msecs"
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

(time
 (do
   (reset! *prime-list* [])
   (create-prime-list-under 2000000)
   (reduce + @*prime-list*)))


;; Problem 11  : 2011/4/11
;;  "Elapsed time: 16.602669 msecs"
(def target-grid 
[[ 8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8]
 [49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0]
 [81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65]
 [52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91]
 [22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80]
 [24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50]
 [32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70]
 [67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21]
 [24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72]
 [21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95]
 [78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92]
 [16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57]
 [86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58]
 [19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40]
 [ 4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66]
 [88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69]
 [ 4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36]
 [20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16]
 [20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54]
 [ 1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48]])

;; test data
(def target-grid 
 [[ 1  2  3  4]
  [11 12 13 14]
  [21 22 23 24]
  [31 32 33 34]])


(defn product-of-4-line [target-lst]
  (loop [[n1 n2 n3 n4 :as tgt] target-lst
     res []]
    (if (< (count tgt) 4)
      res
      (recur (rest tgt) (conj res (* n1 n2 n3 n4) )))))

(defn product-of-4-grid [tgt-grid]
  (map #(product-of-4-line %) tgt-grid))


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
(defn transpose-grid [grid]
  ^{:doc "Transpose grid. "}
  (apply map list grid))
;; macro ver
(defmacro transpose-grid [grid]
  `(map list ~@grid))


;; stagger
;;
;; [[ 1  2  3  4]
;;  [11 12 13 14]
;;  [21 22 23 24]
;;  [31 32 33 34]]
;;   right-->
;; ((1  2  3  4  x  x  x)
;;  (x 11 12 13 14  x  x)
;;  (x  x 21 22 23 24  x)
;;  (x  x  x 31 32 33 34))
;;   left-->
;; (( x  x  x  1  2  3  4)
;;  ( x  x 11 12 13 14  x)
;;  ( x 21 22 23 24  x  x)
;;  (31 32 33 34  x  x  x))
(defn stagger-right [grid]
  (let [add-x (fn [n m lst] (concat (repeat n 'x) lst (repeat m 'x)))]
    (for [line-num (range (count grid))]
      (add-x line-num (dec (- (count grid) line-num)) (nth grid line-num)))))

(defn stagger-left [grid]
  (let [add-x (fn [n m lst] (concat (repeat n 'x) lst (repeat m 'x)))]
    (for [line-num (range (count grid))]
      (add-x (dec (- (count grid) line-num)) line-num (nth grid line-num)))))

;; remove-x
;; ( x  x 11 12 13 14  x) -> (11 12 13 14)
(defn remove-x [coll]
  (remove #(= % 'x) coll))

(apply max
 (map #(apply max %)
      (filter #(not (= % []))
          (concat
           (product-of-4-grid target-grid)
           (product-of-4-grid (transpose-grid target-grid))
           (product-of-4-grid (map #(remove-x %) (transpose-grid (stagger-right target-grid))))
           (product-of-4-grid (map #(remove-x %) (transpose-grid (stagger-left target-grid))))))))



;; Problem 11  : 2011/4/14
;;  "Elapsed time: 1954.309557 msecs"
(use 'clojure.contrib.math)

;; do not work
(defn tri [n]
  (if [= n 1]
    =
    (+ n (tri (dec n)))))


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



;; count divisor
;;  n = f1^a * f2^b * ... * fn^x
;;  divisor count = (a+1)*(b+1)* ... (x+1)
(defn count-divisor [n]
  (let [ ftr (factors n)]
    (reduce *
            (for [tgt (distinct ftr)]
              (inc (count (filter #(= % tgt) ftr)))))))

;; nth triangle num
(defn nth-triangle-num [n]
  (/ (* n (inc n)) 2))



(time 
(loop [n 1]
  (let [triangle-num (nth-triangle-num n)
        divisor-num (count-divisor triangle-num)]
    (if (> divisor-num 500)
      (list n triangle-num divisor-num)
      (recur (inc n)))))
)

(time
 (loop [triangle-num-list (triangle-num)]
   (if (> (count-divisor (first triangle-num-list)) 500)
     (first triangle-num-list)
     (recur (rest triangle-num-list)))))

      

;;; did not use
;; create triangle num
(defn triangle-num
  ([] (triangle-num 2 1))
  ([n sum]
     (lazy-seq
      (cons sum (triangle-num (inc n) (+ n sum))))))

;; check triangle num
(defn triangle-num? [n]
  (let [double-square-int (int (sqrt (* n 2)))
        num-1 double-square-int
        num-2 (inc num-1)]
    (= (/ (* num-1 num-2) 2) n)))





;;12375 76576500 576
;;(factors 76576500)
;; (2 2 3 3 5 5 5 7 11 13 17)
;; (factors 576)
;;(2 2 2 2 2 2 3 3)
;;(2 2 3 3 5 5 5 7 11 13 17)
;;2 2 3 1 1 1 1
;;(* 3 3 4 2 2 2 2)

;; blog style
;; <pre class="brush: clj"> </pre>
;;


