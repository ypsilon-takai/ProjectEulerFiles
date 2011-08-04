;;;
;;;  Project euler  11-20
;;;             Author : Yosi
;;;             From   : 2011/4/12
;;;

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


;; Problem 12  : 2011/4/14
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

;; Problem 13  : 2011/4/14
;; "Elapsed time: 4.569016 msecs"
(def target-list [
		   "37107287533902102798797998220837590246510135740250"
		   "46376937677490009712648124896970078050417018260538"
		   "74324986199524741059474233309513058123726617309629"
		   "91942213363574161572522430563301811072406154908250"
		   "23067588207539346171171980310421047513778063246676"
		   "89261670696623633820136378418383684178734361726757"
		   "28112879812849979408065481931592621691275889832738"
		   "44274228917432520321923589422876796487670272189318"
		   "47451445736001306439091167216856844588711603153276"
		   "70386486105843025439939619828917593665686757934951"
		   "62176457141856560629502157223196586755079324193331"
		   "64906352462741904929101432445813822663347944758178"
		   "92575867718337217661963751590579239728245598838407"
		   "58203565325359399008402633568948830189458628227828"
		   "80181199384826282014278194139940567587151170094390"
		   "35398664372827112653829987240784473053190104293586"
		   "86515506006295864861532075273371959191420517255829"
		   "71693888707715466499115593487603532921714970056938"
		   "54370070576826684624621495650076471787294438377604"
		   "53282654108756828443191190634694037855217779295145"
		   "36123272525000296071075082563815656710885258350721"
		   "45876576172410976447339110607218265236877223636045"
		   "17423706905851860660448207621209813287860733969412"
		   "81142660418086830619328460811191061556940512689692"
		   "51934325451728388641918047049293215058642563049483"
		   "62467221648435076201727918039944693004732956340691"
		   "15732444386908125794514089057706229429197107928209"
		   "55037687525678773091862540744969844508330393682126"
		   "18336384825330154686196124348767681297534375946515"
		   "80386287592878490201521685554828717201219257766954"
		   "78182833757993103614740356856449095527097864797581"
		   "16726320100436897842553539920931837441497806860984"
		   "48403098129077791799088218795327364475675590848030"
		   "87086987551392711854517078544161852424320693150332"
		   "59959406895756536782107074926966537676326235447210"
		   "69793950679652694742597709739166693763042633987085"
		   "41052684708299085211399427365734116182760315001271"
		   "65378607361501080857009149939512557028198746004375"
		   "35829035317434717326932123578154982629742552737307"
		   "94953759765105305946966067683156574377167401875275"
		   "88902802571733229619176668713819931811048770190271"
		   "25267680276078003013678680992525463401061632866526"
		   "36270218540497705585629946580636237993140746255962"
		   "24074486908231174977792365466257246923322810917141"
		   "91430288197103288597806669760892938638285025333403"
		   "34413065578016127815921815005561868836468420090470"
		   "23053081172816430487623791969842487255036638784583"
		   "11487696932154902810424020138335124462181441773470"
		   "63783299490636259666498587618221225225512486764533"
		   "67720186971698544312419572409913959008952310058822"
		   "95548255300263520781532296796249481641953868218774"
		   "76085327132285723110424803456124867697064507995236"
		   "37774242535411291684276865538926205024910326572967"
		   "23701913275725675285653248258265463092207058596522"
		   "29798860272258331913126375147341994889534765745501"
		   "18495701454879288984856827726077713721403798879715"
		   "38298203783031473527721580348144513491373226651381"
		   "34829543829199918180278916522431027392251122869539"
		   "40957953066405232632538044100059654939159879593635"
		   "29746152185502371307642255121183693803580388584903"
		   "41698116222072977186158236678424689157993532961922"
		   "62467957194401269043877107275048102390895523597457"
		   "23189706772547915061505504953922979530901129967519"
		   "86188088225875314529584099251203829009407770775672"
		   "11306739708304724483816533873502340845647058077308"
		   "82959174767140363198008187129011875491310547126581"
		   "97623331044818386269515456334926366572897563400500"
		   "42846280183517070527831839425882145521227251250327"
		   "55121603546981200581762165212827652751691296897789"
		   "32238195734329339946437501907836945765883352399886"
		   "75506164965184775180738168837861091527357929701337"
		   "62177842752192623401942399639168044983993173312731"
		   "32924185707147349566916674687634660915035914677504"
		   "99518671430235219628894890102423325116913619626622"
		   "73267460800591547471830798392868535206946944540724"
		   "76841822524674417161514036427982273348055556214818"
		   "97142617910342598647204516893989422179826088076852"
		   "87783646182799346313767754307809363333018982642090"
		   "10848802521674670883215120185883543223812876952786"
		   "71329612474782464538636993009049310363619763878039"
		   "62184073572399794223406235393808339651327408011116"
		   "66627891981488087797941876876144230030984490851411"
		   "60661826293682836764744779239180335110989069790714"
		   "85786944089552990653640447425576083659976645795096"
		   "66024396409905389607120198219976047599490197230297"
		   "64913982680032973156037120041377903785566085089252"
		   "16730939319872750275468906903707539413042652315011"
		   "94809377245048795150954100921645863754710598436791"
		   "78639167021187492431995700641917969777599028300699"
		   "15368713711936614952811305876380278410754449733078"
		   "40789923115535562561142322423255033685442488917353"
		   "44889911501440648020369068063960672322193204149535"
		   "41503128880339536053299340368006977710650566631954"
		   "81234880673210146739058568557934581403627822703280"
		   "82616570773948327592232845941706525094512325230608"
		   "22918802058777319719839450180888072429661980811197"
		   "77158542502016545090413245809786882778948721859617"
		   "72107838435069186155435662884062257473692284509516"
		   "20849603980134001723930671666823555245252804609722"
		   "53503534226472524250874054075591789781264330331690"])

(time
(reduce + (map #(new BigInteger (apply str (take 13 %))) target-list))
)


;; Problem 14  : 2011/4/15
;; "Elapsed time: 54894.586881 msecs"
;;The following iterative sequence is defined for the set of positive integers:
;;  n Å® n/2 (n is even)
;;  n Å® 3n + 1 (n is odd)
;;Which starting number, under one million, produces the longest chain?
;;NOTE: Once the chain starts the terms are allowed to go above one million.

(defn half-or-triple-plus-one [n]
  (if (= n 1) (list 1)
      (let [next-num (if (even? n) (/ n 2) (+ (* n 3) 1))]
	(lazy-seq
	 (cons n (half-or-triple-plus-one next-num))))))

(defn hotpo-prev-node
  ([] (rev-ho31 1))
  ([n]
     (let [tpo (/ (- n 1) 3)]
       (if (or (<= tpo 1)
	       (not (zero? (mod (- n 1) 3))))
	 (list (* n 2))
	 (list (* n 2)
	       (/ (- n 1) 3))))))

;; memoise?
(let [count-table (atom '{})]
  (defn update-db [num step]
    (swap! count-table assoc num step))
  (defn show-data []
    (println @count-table)))


;; momoise : from clojure.org
(defn memoize [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

;; count hotpo step count
;; 
;; over flow
(defn hotpo-count [n]
  (cond (= n 1) 1
	(even? n) (inc (hotpo-count (/ n 2)))
	:else (inc (hotpo-count (+ (* n 3) 1)))))

;; for memoise
(defn hotpo-count
  ([n] (hotpo-count n 0))
  ([n count]
     (cond (= n 1) (inc count)
	   (even? n) (hotpo-count (/ n 2) (inc count))
	   :else     (hotpo-count (+ (* n 3) 1) (inc count)))))

(def hotpo-count (memoize hotpo-count))

     
     
;; over flow
(defn count-max-hotpo []
  (apply max 
	 (for [target (range 1000000)]
	   (hotpo-count target))))


(loop [n 1000000 max-list [0 0]]
  (if {= n 1)
    max-list
    (let [hotpo (hotopo-count n)]
      (if (< (nth max-list 1) hotpo)
	(recur (dec n) [n hotpo])
	(recur (dec n max-list))))))

(time
(loop [n 1 max-list [0 0]]
  (if (> n 1000000)
    max-list
    (let [hotpo (hotpo-count n)]
      (if (< (nth max-list 1) hotpo)
	(recur (inc n) [n hotpo])
	(recur (inc n) max-list)))))
)

;;[837799 525] : max 2974984576


;; Problem 15  : 2011/4/22
"Elapsed time: 32.748855 msecs"

(use 'clojure.contrib.combinatorics)

;; too long time
(count (combinations (range 40) 20))

(def *grid-x-max* 21)
(def *grid-y-max* 21)
(def *nodes* (atom {}))

;; data
;; {[x y] <grid-node>]}
(defstruct grid-node
  :val :next-node)

(defn disp-node-table [table]
  (partition *grid-x-max*
	     (for [i (range *grid-x-max*) j (range *grid-y-max*)]
	       @(:val (table [i j])))))


(defn get-grid-child [x y]
  (filter #(not (nil? %))
	  (vector (if (< x (dec *grid-x-max*))
		    [(inc x) y])
		  (if (< y (dec *grid-y-max*))
		    [x (inc y)]))))

;; create new node table
;; atom *nodes* ver
(do
  (reset! *nodes* {})
  (for [i (range *grid-x-max*) j (range *grid-y-max*)]
    (let [data (struct grid-node (atom 0) (get-grid-child i j))]
      (swap! *nodes* assoc [i j] data))))


;; NON-atom *nodes* ver
(def *nodes*
     (loop [val-list (for [i (range *grid-x-max*) j (range *grid-y-max*)]
		      (let [data (struct grid-node (atom 0) (get-grid-child i j))]
			[[i j] data]))
	    res-map {}]
       (if (empty? val-list)
	 res-map
	 (recur (rest val-list) (assoc res-map (first (first val-list)) (second (first val-list)))))))
       

;; set initial val
(reset! (:val (*nodes* [0 0])) 1)

;; update child
(defn update-child [parent-val child-list]
  (if (not (empty? child-list))
    (let [target-child (first child-list)]
      (do
	(swap! (:val (*nodes* target-child)) #(+ parent-val %))
	(update-child parent-val (rest child-list))))))


;; dijkstra algorithm
;; atom *nedes* version
(loop [nodes-in-my-hand (vector (@*nodes* [0 0]))]
  (if (empty? nodes-in-my-hand)
    (@*nodes* [(dec *grid-x-max*) (dec *grid-y-max*)])
    (let [target (first nodes-in-my-hand)
	  child-node (:next-node (@*nodes* target))]
      (do
	(update-child @(:val target) child-node)
	(recur (into (rest nodes-in-my-hand) child-node))))))

;; non-atom *nodes* version
(loop [nodes-in-my-hand [[0 0]]]
  (if (not (empty? nodes-in-my-hand))
    (let [target (first nodes-in-my-hand)
	  child-node (:next-node (*nodes* target))]
      (do
;;	(println nodes-in-my-hand)
;;	(println (disp-node-table *nodes*))
	(update-child @(:val (*nodes* target)) child-node)
	(recur (distinct (into (apply vector (rest nodes-in-my-hand)) child-node)))))))

(disp-node-table *nodes*)


;; Problem 16  : 2011/4/22
;; "Elapsed time: 16.475557 msecs"
(use 'clojure.contrib.math)

(loop [num (expt 2 1000)
       result 0]
  (if (< num 1)
    result
    (recur (floor (/ num 10))
           (+ result (rem num 10)))))



;; Problem 17  : 2011/4/22
;; "Elapsed time: 106.548179 msecs"
(use 'clojure.contrib.math)
(def number-table
  '{ 1        "one"
     2        "two"
     3        "three"
     4        "four"
     5        "five"
     6        "six"
     7        "seven"
     8        "eight"
     9        "nine"
     10        "ten"
     11        "eleven"
     12        "twelve"
     13        "thirteen"
     14        "fourteen"
     15        "fifteen"
     16        "sixteen"
     17        "seventeen"
     18        "eihgteen"
     19        "nineteen"
    20         "twenty"
    30         "thirty"
    40         "forty"
    50         "fifty"
    60         "sixty"
    70         "seventy"
    80         "eighty"
    90         "ninety"
    100        "hundred" })

(defn writedown-tys [n]
  (cond (<= n 0) nil
        (< n 20)
          (number-table n)
        :else
          (apply str (interpose " "
                    (list (number-table (* 10 (floor (/ n 10))))
                          (number-table (rem n 10)))))))

(defn writedown-hdrs [n]
  (cond (<= n 0) nil
        :else (apply str (interpose " "
                      (list (number-table n)
                            "hundred")))))
        
(defn writedown [n]
  (if (= n 1000)
    "one thousand"
  (let [hundreds (floor (/ n 100))
        tys (rem n 100)]
    (apply str
           (interpose " "
                      (filter #(not (nil? %))
                              (list
                               (writedown-hdrs hundreds)
                               (if (and (> hundreds 0) (> tys 0))
                                 "and"
                                 nil)
                               (writedown-tys tys))))))))

        
(loop [nums (range 1 1001) total 0]
  (if (empty? nums)
    total
    (recur (rest nums)
           (+ total
              (.length (.replace (writedown (first nums)) " " ""))))))


;; pattern 2
(def number-table
     '{0  0
       1  3
       2  3
       3  5
       4  4
       5  4
       6  3
       7  5
       8  5
       9  4
       10  3
       11  6
       12  6
       13  8
       14  8
       15  7
       16  7
       17  9
       18  8
       19  8
       20  6
       30  6
       40  5
       50  5
       60  5
       70  7
       80  6
       90  6
       100 7})


(defn word-count-tys [n]
  (cond (<= n 0) nil
        (< n 20)
          (list (number-table n))
        :else
        (list (number-table (* 10 (floor (/ n 10))))
              (number-table (rem n 10)))))


(defn word-count-hdrs [n]
  (cond (<= n 0) nil
        :else (list (number-table n)
                    7)))


(defn word-count-list [n]
  (if (= n 1000)
    (list '(3 8))
  (let [hundreds (floor (/ n 100))
        tys (rem n 100)]
    (filter #(not (nil? %))
            (list
             (word-count-hdrs hundreds)
             (if (and (> hundreds 0) (> tys 0))
               '(3)
               nil)
             (word-count-tys tys))))))

        
(loop [nums (range 1 1001) total 0]
  (if (empty? nums)
    total
    (recur (rest nums)
           (+ total
              (reduce + (map #(apply + %) (word-count-list (first nums))))))))

(loop [nums (range 1 1001)]
  (if (empty? nums)
    nil
    (do
      (println            (word-count-list (first nums)))
      (recur (rest nums)))))


;; Problem 18  : 2011/4/27
;; "Elapsed time: 9.528585 msecs"
(def org-data '((75)
               (95 64)
               (17 47 82)
               (18 35 87 10)
               (20  4 82 47 65)
               (19  1 23 75  3 34)
               (88  2 77 73  7 63 67)
               (99 65  4 28  6 16 70 92)
               (41 41 26 56 83 40 80 70 33)
               (41 48 72 33 47 32 37 16 94 29)
               (53 71 44 65 25 43 91 52 97 51 14)
               (70 11 33 28 77 73 17 78 39 68 17 57)
               (91 71 52 38 17 14 91 43 58 50 27 29 48)
               (63 66  4 68 89 53 67 30 73 16 69 87 40 31)
               ( 4 62 98 27 23  9 70 98 73 93 38 53 60  4 23)))

;; test data
(def org-data '(
                (75)
                (95 64)
                (17 47 82)
                (18 35 87 10)))


(defstruct grid-node
  :weight :val)

(defn make-line-data [node-list target-list res-map]
  (if (empty? node-list)
    res-map
    (let [node (first node-list) target-data (first target-list)]
      (recur (rest node-list) (rest target-list)
             (assoc res-map node (struct grid-node target-data (atom target-data)))))))

(defn create-node-data [n]
  (for [i (range n)]
    [i (dec (- n i))]))

(def *nodes*
     (loop [grid-data org-data
            res-map {}]
       (if (empty? grid-data)
         res-map
         (let [target-list (first grid-data)
               node-data (create-node-data (count target-list))]
           (recur (rest grid-data) (make-line-data node-data target-list res-map))))))

(defn show-data []
  (loop [stage (range (count org-data) 0 -1)
        res-list ()]
    (if (empty? stage)
      res-list
      (recur
       (rest stage)
       (cons (map #(deref (:val (*nodes* %))) (create-node-data (first stage))) res-list)))))


(defn update-child [point]
     (let [value @(:val (*nodes* point))
           child-1 [(inc (first point)) (second point)]
           ch-1-weight (:weight (*nodes* child-1))
           child-2 [(first point) (inc (second point))]
           ch-2-weight (:weight (*nodes* child-2))
           ]
       (do
        (swap! (:val (*nodes* child-1)) #(max (+ value %2) %1) ch-1-weight)
        (swap! (:val (*nodes* child-2)) #(max (+ value %2) %1) ch-2-weight))))

                        
(loop [stage (range 1 15)]
  (if (empty? stage)
    nil
    (do
      (dorun (for [target (create-node-data (first stage))]
               (update-child target)))
      (recur (rest stage)))))




;; Problem 19  : 2011/4/27
;; "Elapsed time: 0.492521 msecs" 1900-1900
;; "Elapsed time: 1.898565 msecs" 1900-2000

(def month-non-leap [31 28 31 30 31 30 31 31 30 31 30 31])
(def month-leap [31 29 31 30 31 30 31 31 30 31 30 31])

(def week-day '{0 "sun" 1 "mon" 2 "tue" 3 "wed" 4 "thu" 5 "fri" 6 "sat" })

(defn leap-year? [year]
  (cond (not (zero? (rem year 4))) false
        (zero? (rem year 400)) true
        (zero? (rem year 100)) false
        :else true))

(defn get-month [is-leap]
  (if is-leap
    month-leap
    month-non-leap))

(defn first-days [is-leap first-day]
  (loop [month-list (get-month is-leap)
         res-list (list first-day)]
    (if (empty? month-list)
      res-list
      (let [month-day (first month-list)]
        (recur (rest month-list)
               (cons (rem (+ (first res-list) month-day) 7) res-list))))))

(defn count-sunday [start-year end-year first-day]
  (loop [year-list (range start-year (inc end-year))
         first-day first-day
         sunday-count 0]
    (if (empty? year-list)
      sunday-count
      (let [[next-first & data] (first-days (leap-year? (first year-list)) first-day)]
        (recur (rest year-list)
               next-first
               (+ sunday-count (count (filter #(= % 0) data))))))))

(-
 (count-sunday 1900 2000 1)
 (count-sunday 1900 1900 1))




;; Problem 20  : 2011/4/27
;; "Elapsed time: 7.363226 msecs"

;; momoise : from clojure.org
(use 'clojure.contrib.math)

(defn memoize [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(defn fact [n]
  (if (= n 1)
   1
   (* n (fact (dec n)))))

(def fact (memoize fact))
(map fact (range 1 100))

(loop [num (fact 100)
       result 0]
  (if (< num 1)
    result
    (recur (floor (/ num 10))
           (+ result (rem num 10)))))


    
