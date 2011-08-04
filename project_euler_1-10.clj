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




