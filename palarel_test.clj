
(use 'clojure.contrib.math)

(let [nums 10000]
  (time 
   (reduce + (map (fn [x] (rem (expt 2 x) 100)) (repeat 10000 nums)))))
"Elapsed time: 5561.606014 msecs"


(let [nums 10000]
  (time 
   (reduce + (pmap (fn [x] (rem (expt 2 x) 100)) (repeat 10000 nums)))))
"Elapsed time: 3480.631756 msecs"


(let [nums 10000]
  (time 
   (reduce + (pmap #(reduce + (map (fn [x] (rem (expt 2 x) 100)) %))
		   (partition 10 (repeat 10000 nums))))))
"Elapsed time: 3255.111753 msecs"

(let [nums 10000]
  (time 
   (reduce + (pmap #(reduce + (map (fn [x] (rem (expt 2 x) 100)) %))
		   (partition 100 (repeat 10000 nums))))))
"Elapsed time: 3099.338324 msecs"

(let [nums 10000]
  (time 
   (reduce + (pmap #(reduce + (map (fn [x] (rem (expt 2 x) 100)) %))
		   (partition 1000 (repeat 10000 nums))))))
"Elapsed time: 3064.371031 msecs"

(let [nums 10000]
  (time 
   (reduce + (pmap #(reduce + (map (fn [x] (rem (expt 2 x) 100)) %))
		   (partition 5000 (repeat 10000 nums))))))
"Elapsed time: 3092.340227 msecs"




