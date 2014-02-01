(ns intro-ex.core
  (:use [intro-ex.random-seqs])
  (:require [incanter [core :as incc] [charts :as ichart]])
  (:gen-class))

(def random-dists-cnt
  "Number of random distributions to generate"
  50)

(def seqs-per-p
  "Number of random sequences to generate"
  300)

(def seq-len
  "Length of the random sequences to generate for the first plot"
  5)

(def orig-p
 "The probability distribution from which the sequences shall be made"
  { \a 0.5 \b 0.2 \c 0.3 })

(def random-ps
  "Random probability distributions"
  (repeatedly random-dists-cnt #(gen-random-distribution [\a \b \c])))

(def divergences
  "Divergences of the random distributions from the original one"
  (map (partial kullback-leibler-div orig-p) random-ps))

(def random-seqs
  "Random sequences generated according to the original distribution"
  (repeatedly seqs-per-p #(gen-random-seq orig-p seq-len)))

(def orig-probs
  "Probabilities of the random sequences"
  (map (partial prob-of-seq orig-p) random-seqs))

(def orig-log-probs
  "Log probabilities of the random sequences"
  (map (partial log-prob-of-seq orig-p) random-seqs))

(def rand-p-probs
  "Probabilities of the random sequences in case they come from the
   random distributions like this:

   [[Pr(s1, p1) Pr(s2, p1) ... Pr(sn, p1)]
    [Pr(s1, p2) ...                      ]
    ...                                   ]"
  (map (fn [p]
         (map (partial prob-of-seq p) random-seqs))
       random-ps))

(def prob-ratios
  "Ratio between the original probability and the probability under the
   random distributions for every random sequence"
  (map (fn [probs]
         (map / orig-probs probs))
       rand-p-probs))

(def divergence-ratio-plot
  "Scatterplot of Kullback-Leibler divergences of random probability
  distributions from the original probability distribution in dependence of
  ratios between probability of sequences under those distributions."
      (ichart/scatter-plot
          (apply concat prob-ratios)
          (mapcat (partial repeat seqs-per-p) divergences)))

(defn -main [& args]
  (incc/view
    (ichart/set-x-range divergence-ratio-plot 0 10)
    :height 700
    :width  1000))
