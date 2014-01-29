(ns intro-ex.core
  (:gen-class))

; TODO: Create a proper abstraction for intervals.

(def dist1 { \a 0.3 \b 0.3 \c 0.4 })

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

(defn gen-random-seq [length distribution]
  "Returns a random seq of length length formed from keys of
  distribution map. distribution maps keys to the probability with which
  they should occur."
  {:pre [(pos? length)
         (prob-sum-1? distribution)]}
  (let [key-to-interval (distr-to-intervals distribution) 
        random-nums     (rand-float-seq length)]
    (map (partial num-to-key key-to-interval) random-nums)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
