;;;
;;;  Project euler  51-60
;;;             Author : Yosi
;;;             From   : 2011/6/15
;;;


;; Problem 51  : 2011/6/15
;; "Elapsed time: 2729.218378 msecs"

(def tgt-prime (drop-while #(< % 1000) (prime-nums-under 1000000)))

(defn list-to-num [digit-list]
  (apply + (map #(* %1 (expt 10 %2)) (reverse digit-list) (iterate inc 0))))

(defn num-to-list [num]
  (map #(Character/digit % 10) (str num)))

(defn replace-digits [num digit]
  (for [rep (range 0 10)]
    (list-to-num (map #(if (= % digit) rep %) (num-to-list num)))))

(defn group-same
  "Split into same value group
    ex. (2 3 1 3 1 3 3) -> [(1 1) (2) (3 3 3 3)]"
  ([col] (group-same (sort col) []))
  ([col res]
     (if (empty? col)
       res
       (let [[head tail] (split-with #(= (first col) %) col)]
	 (recur tail (conj res head ))))))

;;; original
(filter (fn [[digit-of-three num]]
	  (> (count (filter is-prime? (replace-digits num digit-of-three))) 7))
	(map (fn [[count-list num]]
	       [(last (first  count-list)) num])
	     (filter (fn [[tst _]] (some #(= 3 (first %)) tst))
		     (map (fn [prime]
			    (list (map #(list (count %) (first %))
				       (group-same (butlast (num-to-list prime))))
				  prime))
			  tgt-prime))))

;; macro version
(->> tgt-prime
     (map (fn [prime]
	    (list (map #(list (count %) (first %)) (group-same (butlast (num-to-list prime))))
		  prime)), )

     (filter (fn [[tst _]] (some #(= 3 (first %)) tst)), )     

     (map (fn [[count-list num]]
	    [(last (first countlist)) num]), )

     (filter (fn [[digit-of-three num]]
	       (< 7 (count (filter is-prime? (replace-digits num digit-of-three))))), ))


(857 111857 222857 333857 555857 666857 777857 888857)
(121313 222323 323333 424343 525353 626363 828383 929393)
(40609 141619 242629 343639 444649 646669 747679 949699)

     


;; Problem 52  : 2011/6/15
;; "Elapsed time: 3178.80848 msecs"

(defn same-digits? [n m]
  (= (sort (num-to-list n))
     (sort (num-to-list m))))

(defn pe52-end-num [digs]
  (+ (expt 10 digs)
     (list-to-num (repeat digs 6))))

(take 1
(drop-while empty?
(for [digs (iterate inc 5)]
  (filter #(and (same-digits? % (* 2 %))
		(same-digits? % (* 3 %))
		(same-digits? % (* 4 %))
		(same-digits? % (* 5 %))
		(same-digits? % (* 6 %)))
	  (range (+ (expt 10 digs) 2) (inc (pe52-end-num digs)) 3)))))


;; Problem 53  : 2011/6/16
;; "Elapsed time: 473.81832 msecs"

(defn fact [n]
  (reduce * (range 1 (inc n))))

(defn composision [n r]
  (/ (fact n) (* (fact r) (fact (- n r)))))


(count
 (filter #(> % 1000000)
	 (for [n (range 2 101) r (range 1 100) :when (> n r)]
	   (composision n r)))))

(let [n 20]
  (for [r (range 1 n)]
    (composision n r)))
    



;; Problem 54  : 2011/6/20
;; "Elapsed time: 430.744131 msecs"

(use '[clojure.contrib.duck-streams :only (reader read-lines)])
(use '[clojure.contrib.str-utils :only (re-split)])

(defn group-same
  "Split into same value group
    ex. (2 3 1 3 1 3 3) -> [(1 1) (2) (3 3 3 3)]"
  ([col] (group-same (sort col) []))
  ([col res]
     (if (empty? col)
       res
       (let [[head tail] (split-with #(= (first col) %) col)]
	 (recur tail (conj res head ))))))


(def card-value
     {\2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, 
      \9 9, \T 10, \J 11, \Q 12, \K 13, \A 14})

(def suite-value
     {\C :club, \H :heart, \D :diamond, \S :spade})

(def hand-rank
     {:high-card 0 :one-pair 1 :two-pairs 2 :three-ofa-kind 3 :straight 4 :flush 5
      :full-house 6 :four-ofa-kind 7 :straight-flush 8 :royal-flush 9})

(defn expand-card [note]
  "extract notation to data 8C -> [8 :club]"
  (let [[n s] (seq note)]
    [(card-value n) (suite-value s)]))



(defn how-many-pairs [hand]
  (count (filter #(= % 2) (map count (group-same (map first hand))))))

(defn how-many-3ok [hand]
  (count (filter #(= % 3) (map count (group-same (map first hand))))))

(defn how-many-4ok [hand]
  (count (filter #(= % 4) (map count (group-same (map first hand))))))

(defn one-pair? [hand]
  (= 1 (how-many-pairs hand)))

(defn two-pair? [hand]
  (= 2 (how-many-pairs hand)))

(defn three-of-kind? [hand]
  (= 1 (how-many-3ok hand)))

(defn straight? [hand]
  (apply = (map #(- %1 %2) (sort (map first hand)) (range 5))))

(defn flush? [hand]
  (apply = (map second hand)))

(defn full-house? [hand]
  (and (= 1 (how-many-pairs hand))
       (= 1 (how-many-3ok hand))))

(defn four-of-kind? [hand]
  (= 1 (how-many-4ok hand)))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn royal-flush? [hand]
  (and (flush? hand)
       (= (sort (map first hand)) '(10 11 12 13 14))))

(defn rank-hand [hand]
  (cond (royal-flush? hand) :royal-flush
	(straight-flush? hand) :straight-flush
	(four-of-kind? hand) :four-ofa-kind
	(full-house? hand) :full-house
	(flush? hand) :flush
	(straight? hand) :straight
	(three-of-kind? hand) :three-ofa-kind
	(two-pair? hand) :two-pairs
	(one-pair? hand) :one-pair
	:else :high-card))

(defn poker-sort [hand]
  "Sort cand num. set letf according to card count.
   ex. [2 2 3 7 7 8] -> (7 2 8 3) : 7 and 2 are two cards."
  (map first
  (sort (fn [a b]
	  (if (= (count a) (count b))
	    (> (first a) (first b))
	    (> (count a) (count b))))
	(group-same (map first hand)))))

(defn compare-poker [p1 p2]
  "compare porker-sorted cards." 
  (if (= (first p1) (first p2))
    (recur (rest p1) (rest p2))
    (if (> (first p1) (first p2))
      :p1
      :p2)))
	 

(defn which-is-win [[p1-hand p2-hand]]
  (let [p1-rank (hand-rank (rank-hand p1-hand))
	p2-rank (hand-rank (rank-hand p2-hand))]
    (cond (> p1-rank p2-rank) :p1
	  (< p1-rank p2-rank) :p2
	  :else
	  (let [p1-sort (poker-sort p1-hand)
		p2-sort (poker-sort p2-hand)]
	    (compare-poker p1-sort p2-sort)))))

;; one hand data
;;[[8 :club] [10 :spade] [13 :club] [8 :heart] [4 :spade]]

(count (filter #(= :p1 %)
(let [file-data (read-lines "D:/Profiles/q3197c/Desktop/poker_test.txt")
      input-datas (map #(split-at 5 (map expand-card (re-split #"\s+" %))) file-data)]
  (map which-is-win input-datas))))




;; Problem 55  : 2011/6/20
;; "Elapsed time: 9821.708625 msecs"

(defn reverse-and-add-if-not-palindrome [n]
  (let [revnum (list-to-num (reverse (num-to-list n)))]
    (if (= revnum n)
      true
      (+ n revnum))))

(defn lychrel? [n]
  (loop [depth 1 num (+ n (list-to-num (reverse (num-to-list n))))]
    (let [next-data (reverse-and-add-if-not-palindrome num)]
      (cond (>= depth 50) false
	    (true? next-data) true
	    :else (recur (inc depth) next-data)))))x


(count (filter false? (map lychrel? (range 1 10000))))



;; Problem 56  : 2011/6/20
;; "Elapsed time: 15482.925064 msecs"

(reduce max
	(for [a (range 1 100) b (range 1 100)]
	  (reduce + (num-to-list (expt a b)))))



;; Problem 57  : 2011/6/21
;; "Elapsed time: 4859.538293 msecs"
(defn cf-seq [n]
  (if (zero? n)
    1
    2))

(defn cf-numerator [n]
  (do (println "-->" n)
  (cond (zero? n) 1
	(= 1 n) (cf-seq 0)
	:else (+ (* (cf-seq (dec n))(cf-numerator (dec n))) (cf-numerator (- n 2))))))

(defn cf-denominator [n]
  (cond (zero? n) 0
	(= 1 n) 1
	:else (+ (* (cf-seq (dec n))(cf-denominator (dec n))) (cf-denominator (- n 2)))))

(def cf-numerator (memoize cf-numerator))
(def cf-denominator (memoize cf-denominator))

;;;;;!!!!!

(defn-memo cf-numerator [n]
  (cond (zero? n) 1
	(= 1 n) (cf-seq 0)
	:else (+ (* (cf-seq (dec n))(cf-numerator (dec n))) (cf-numerator (- n 2)))))

(defn-memo cf-denominator [n]
  (cond (zero? n) 0
	(= 1 n) 1
	:else (+ (* (cf-seq (dec n))(cf-denominator (dec n))) (cf-denominator (- n 2)))))


(defn pe57? [n]
  (> (count (num-to-list (cf-numerator n)))
     (count (num-to-list (cf-denominator n)))))


(count (filter true? (pmap pe57? (range 20))))



;; Problem 58  : 2011/6/21
"Elapsed time: 3646.419866 msecs"

(defn corner-set [n]
  (let [side-len (- (* n 2) 1)
	bottom-right (expt side-len 2)
	bottom-left (- bottom-right (- side-len 1))
	top-left (- bottom-right (* 2 (- side-len 1)))
	top-right (- bottom-right (* 3 (- side-len 1)))]
    [top-right top-left bottom-left bottom-right]))



(loop [n 2
       prime-count 0]
  (let [new-prime-count (+ prime-count
			   (count (filter is-prime? (butlast (corner-set n)))))
	corner-count (- (* n 4) 3)
	prime-ratio (/ new-prime-count corner-count)]
    (if (< prime-ratio 1/10)
      [n (- (* n 2) 1)]
      (recur (inc n) new-prime-count))))
    


;; Problem 59  : 2011/6/22
;;"Elapsed time: 37.556729 msecs"

(defn group-same
  "Split into same value group
    ex. (2 3 1 3 1 3 3) -> [(1 1) (2) (3 3 3 3)]"
  ([col] (group-same (sort col) []))
  ([col res]
     (if (empty? col)
       res
       (let [[head tail] (split-with #(= (first col) %) col)]
	 (recur tail (conj res head ))))))

(defn transpose-grid 
  "Transpose grid.
     [[1 2]
      [3 4]] ->
     [[1 3]
      [2 4]]"
  [grid]
  (apply map list grid))


(defn sort-by-count [num-list]
  "Sort num. set letf according to the count of the number.
   ex. [2 2 3 7 7 8] -> (7 2 8 3) : 7 and 2 are two cards."
  (map first
       (sort (fn [a b]
	       (if (= (count a) (count b))
		 (> (first a) (first b))
		 (> (count a) (count b))))
	     (group-same num-list))))


;;;

(def *pe59-file-name* "D:/Profiles/q3197c/Desktop/cipher1.txt")
(def *pe59-file-name* "http://projecteuler.net/project/cipher1.txt")

(use '[clojure.contrib.io :only (slurp*)])
(use '[clojure.contrib.str-utils :only (re-split chomp)])

(defn get-num-seq [file-name]
  (let [file-data (slurp* file-name)]
    (map #(Integer/valueOf %) (re-split #"," (chomp file-data)))))

(defn encdec-char [key val]
  (if (neg? val)
    0
    (bit-xor key val)))

(defn get-decode-pe59 [key]
  (let [[k1 k2 k3] (map int (seq key))]
    (reduce str
    (reduce concat
    (map #(list (char (encdec-char k1 (first %)))
		(char (encdec-char k2 (second %)))
		(char (encdec-char k3 (last %))))
	 (partition 3 (get-num-seq *pe59-file-name*)))))))


(defn pe59 [key]
  (let [[k1 k2 k3] (map int (seq key))]
	    (map #(+ (encdec-char k1 (first %))
		     (encdec-char k2 (second %))
		     (encdec-char k3 (last %)))
		 (partition 3 3 (repeat -1) (get-num-seq *pe59-file-name*)))))


(let [most-frequent-char \space]
  (reduce str
	  (map #(char (encdec-char % (int most-frequent-char)))
	       (map first
		    (map sort-by-count
			 (transpose-grid (partition 3 3 (repeat -1) (get-num-seq *pe59-file-name*))))))))



;; Problem 60  : 2011/6/
;;

(use 'clojure.contrib.math)
(use '[clojure.contrib.seq-utils :only (includes?)])


(def primes (get-prime-list))


(defn parting-into-prime-pairs [n]
  "get all prime pairs of parting number. ex. 24113 -> ([2411 3] [241 13]) "
  (filter (complement empty?)
  (for [split-point (iterate #(* 10 %) 10) :while (<= split-point n)]
    (let [l (rem n split-point) u (/ (- n l) split-point)]
      (if (and (> l (/ split-point 10))    ;; take care of 0
	       (is-prime? u)
	       (is-prime? l))
	  [u l]
	  [])))))

; parted prime number list : [<primes> ([<prime list>]...)
(def parted-primes
     (filter #(not (empty? (second %)))
	     (map #(vector % (parting-into-prime-pairs %)) primes)))


;; (def pair-map (atom {}))
;;
;; (loop [data devidable-all]
;;     (if (empty? data)
;;       true
;;       (do (let [[base pair-list] (first data)]
;; 	    (dorun (map (fn [pair]
;; 			  (let [res (get @pair-map (sort pair) ())]
;; 			    (swap! pair-map assoc (sort pair) (cons base res))))
;; 			pair-list))
;; 	    (recur (rest data))))))


;; list of prime pair they can concatenate into prime(s) in normal and/or reverse order.
;; [(83 563) (83563 56383)]
(def pair-map
(reduce (fn [prime-num dat]
	  (let [[pnum pair-list] dat]
	    (reduce (fn [pn pair]
		      (let [res (get pn (sort pair) ())]
			(assoc pn (sort pair) (cons pnum res))))
		    prime-num pair-list)))
	    {} parted-primes))

(def full-prime-pair
(for [map-key (keys pair-map) :when (> (count (get pair-map map-key)) 1)]
  map-key))


(def full-prime-pair-set
     (set full-prime-pair))

(defn prime-pair? [a b]
  (contains? full-prime-pair-set [a b]))

(def prime-pair-list
     (filter #(> (count (second %)) 4)
	     (reduce (fn [m lst]
		       (let [k1 (get m (first lst) ())
			     k2 (get m (second lst) ())]
			 (assoc m (first lst) (cons (second lst) k1)
				(second lst) (cons (first lst) k2))))
		     {} full-prime-pair)))

(mapcat (fn [[p col]]
	  (for [a col b col :when (not= a b)]
	    (if (prime-pair? [a b])
	      [a b]
	      [])))
	'([97 (6271 8287 1951 1279 1021 4957 829 9949 6301 157 9787 4507 379 7 9337 919 7927 1783 43 8053 373 5011 883 787 9907 7507 8209 2833 4657 8689 241 1039 6991 19 3727 8269 3853 6637 5581 3373 2221 8011 3691 6571 1291 4969 8521 1063 967 1093 1381 4261 6883 3331 2017 7681 4513 5281)]))
       
		     

			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new

(defn concat-num [n m]
  (let [tenth (first (drop-while #(<= % m) (iterate #(* 10 %) 1)))]
    (+ (* n tenth) m)))

(defn prime-pair? [n m]
  (and (is-prime? (concat-num n m))
       (is-prime? (concat-num m n))))

(defn all-make-prime-pair? [p-set p]
  (every? true? (map #(prime-pair? p %) p-set)))

(defn all-child-nodes [tgt-sets]
   (for [t-set tgt-sets
	 new-prime (next (take 100000 primes))  ; remove 2
	 :when (and (> new-prime (reduce max t-set))
		    (not (contains? t-set new-prime))
		    (all-prime-pair? t-set new-prime))]
   (conj t-set new-prime)))

(defn all-child-nodes [tgt-sets]
   (for [new-prime (next primes)  ; remove 2
	 t-set tgt-sets
	 :when (and (> new-prime (reduce max t-set))
		    (not (contains? t-set new-prime))
		    (all-prime-pair? t-set new-prime))]
   (conj t-set new-prime)))

;; needs too long time
(take 1 (all-child-nodes
	 (all-child-nodes
	  (all-child-nodes
	   (all-child-nodes
	    (map sorted-set primes))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new 2
(use 'clojure.contrib.math)
(use '[clojure.contrib.seq-utils :only (includes?)])


(def primes (get-prime-list))

(defn concat-num [n m]
  (let [tenth (first (drop-while #(<= % m) (iterate #(* 10 %) 1)))]
    (+ (* n tenth) m)))

(defn prime-pair? [n m]
  (and (is-prime? (concat-num n m))
       (is-prime? (concat-num m n))))

(defn all-make-prime-pair? [p-set p]
  (every? true? (map #(prime-pair? p %) p-set)))

(defn next-child-node [tgt-list last-prime]
  (let [new-prime (first (filter (fn [cand]
				   (all-make-prime-pair? tgt-list cand))
				 (drop-while #(<= % last-prime) primes)))]
    (if (nil? new-prime)
      false
      (conj tgt-list new-prime))))

(defn pe60 [seq-count]
  (loop [all-nodes (apply sorted-set-by
			  (fn [x y] (< (reduce + x) (reduce + y)))
			  (map list (next primes)))
	 last-creation {}]
    (let [tgt-node (first all-nodes)
	  last-created-prime (get last-creation tgt-node 1)]
      (if-let [new-node (next-child-node tgt-node last-created-prime)]
	(if (or (>= (count new-node) seq-count) (> (count all-nodes) 30))
	  new-node
	  (do (println "1-> " all-nodes)
	      (recur (conj all-nodes new-node)
		     (assoc last-creation tgt-node (reduce max new-node)))))
	(do (println "2-> " all-nodes)
	    (println "2-> " tgt-node)
	    (recur (disj all-nodes tgt-node)
		   (dissoc last-creation tgt-node)))))))


(defn pe60 [seq-count]
  (loop [all-nodes (apply sorted-set-by
			  (fn [x y] (< (reduce + x) (reduce + y)))
			  (map list (next primes)))
	 last-creation {}]
    (let [tgt-node (first all-nodes)
	  last-created-prime (get last-creation tgt-node 1)]
      (if-let [new-node (next-child-node tgt-node last-created-prime)]
	(if (>= (count new-node) seq-count)
	  new-node
	  (recur (conj all-nodes new-node)
		 (assoc last-creation tgt-node (reduce max new-node))))
	(recur (disj all-nodes tgt-node)
	       (dissoc last-creation tgt-node))))))

    

	
       
      

