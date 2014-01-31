(ns intro-ex.core
  (:gen-class))

; TODO: Create a proper abstraction for intervals.

(def dist-p  { \a 0.3 \b 0.3 \c 0.4 })
(def dist-p' { \a 0.5 \b 0.2 \c 0.3 })

(defn- rand-float-seq [length]
  "Returns a seq of length length of random floating point numbers."
  {:pre [(pos? 0)]}
  (repeatedly length rand))

(defn- in-interval? [x [lower upper]]
  "Returns true if x is in the right-open interval from lower to upper."
  {:pre [(< lower upper)]}
  (and (<= lower x) (< x upper)))

(defn- prob-sum-1? [distribution]
  "Returns true if the sum of the values (probabilities) in distribution
  is 1."
  (= 1 (apply + (vals distribution))))

(defn- distr-to-intervals [distribution]
  "distribution maps keys to probabilities. Returns a similar map, but
  from keys to right-open subintervals of [0, 1[. The size of the
  subinterval of one key is equal to its probability. The subintervals
  form a partition of [0, 1[. (Note that we don't have real numbers
  here, so these things shouldn't be taken too exactly."
  {:pre [(prob-sum-1? distribution)]}
  (loop [rem-keys   (keys distribution)
         intervals  {}
         last-upper 0]
   (if (empty? rem-keys)
     intervals
     (let [cur-key    (first rem-keys)
           cur-upper  (+ last-upper (distribution cur-key))]
       (recur 
         (rest rem-keys) 
         (assoc intervals cur-key [last-upper cur-upper])
         cur-upper)))))

(defn- num-to-key [interval-for x]
  "Returns the key belonging to the interval whose element is x. If the
  intervals are not disjunct and x is in two of them, no assumption can
  be made about the returned key."
  (some (fn [k] (if (in-interval? x (interval-for k)) k))
        (keys interval-for)))

(defn- make-differences [bounds]
  "bounds are distinct numbers between 0 and 1. Returns a coll of the
  lengths of the intervals specified by the bounds."
  {:pre  [(apply distinct? bounds)
          (every? #(< 0 % 1) bounds)]}  
;   :post [(= 1 (apply + %))]}  % doesn't work for some reason.
  (let [subtrahends (cons 0 (sort bounds))
        minuends    (lazy-cat (rest subtrahends) [1])]
    (map - minuends subtrahends)))

(defn gen-random-distribution [ks]
  "Returns a random probability distribution for the keys ks. 
  
  It is not clear what a random probability distribution should look
  like.  Currently this function finds some random numbers between 0 and
  1 and uses the differences between them as probabilities.
 
  Note also, that this function will assign a non-zero probability to
  every key in ks. Therefore it is not guaranteed, only very likely,
  that it will terminate."
  {:pre  [(seq ks)]}
;   :post [(prob-sum-1? %)]}  % doesn't work for some reason.
  (let [bounds (rand-float-seq (dec (count ks)))]
    (if (and (not (apply distinct? bounds)) (not-any? zero? bounds))
      (gen-random-distribution ks)
      (zipmap ks (make-differences bounds)))))

(defn gen-random-seq [distribution length]
  "Returns a random seq of length length formed from keys of
  distribution map. distribution maps keys to the probability with which
  they should occur."
  {:pre [(pos? length)
         (prob-sum-1? distribution)]}
  (let [interval-for (distr-to-intervals distribution) 
        random-nums  (rand-float-seq length)]
    (map (partial num-to-key interval-for) random-nums)))

(defn prob-of-seq [probability-for coll]
  "Returns the probability of coll to come from a random collection
  generator with probability distribution probability-for."
  {:pre [(prob-sum-1? probability-for)
         (seq coll)]}
  (apply * (map probability-for coll)))

(defn kullback-leibler-div [prob-for1 prob-for2]
  "Returns the Kullback-Leibler divergence of the probability
  distribution prob-for2 from prob-for1."
  {:pre [(prob-sum-1? prob-for1)
         (prob-sum-1? prob-for2)
         (= (keys prob-for1) (keys prob-for2))]}
  (apply + (map (fn [k] (* (Math/log (/ (prob-for1 k) (prob-for2 k)) )
                           (prob-for2 k))) 
                (keys prob-for1))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
