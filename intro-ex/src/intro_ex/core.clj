(ns intro-ex.core
  (:use [intro-ex.random-seqs])
  (:require [incanter [core :as incc] [charts :as ichart]])
  (:gen-class))

(def random-dists-cnt 50)
(def seqs-per-p 300)
(def seq-len 5)

; TODO: The comments shoud probably go in the definitions.

; The probability distribution from which the sequences shall be made
(def orig-p { \a 0.5 \b 0.2 \c 0.3 })

; Random probability distributions
(def random-ps (repeatedly random-dists-cnt
                           #(gen-random-distribution [\a \b \c])))

; Divergences of the random distributions from the original one
(def divergences (map (partial kullback-leibler-div orig-p) random-ps))

; Random sequences generated according to the original distribution
(def random-seqs (repeatedly
                   seqs-per-p
                   #(gen-random-seq orig-p seq-len)))

; Probabilities of the random sequences
(def orig-probs (map (partial prob-of-seq orig-p) random-seqs))

; Probabilities of the random sequences in case they come from the
; random distributions like this:
;
; [[Pr(s1, p1) Pr(s2, p1) ... Pr(sn, p1)]
;  [Pr(s1, p2) ...                      ]
;  ...                                   ]
(def rand-p-probs (map
                    (fn [p] (map (partial prob-of-seq p) random-seqs))
                    random-ps))

; Ratio between the original probability and the probability under the
; random distributions for every random sequence
(def prob-ratios (map
                   (fn [probs] (map / orig-probs probs))
                   rand-p-probs))

(defn -main [& args]
  (incc/view
    (ichart/set-x-range
      (ichart/scatter-plot
          (apply concat prob-ratios)
          (mapcat (partial repeat seqs-per-p) divergences))
      0 10)
  :height 700
  :width  1000))
