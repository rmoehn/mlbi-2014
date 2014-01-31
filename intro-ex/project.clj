(defproject intro-ex "0.1.0-SNAPSHOT"
  :description "Solutions to the exercises in MACHINE LEARNING
               INTRODUCTION: STRING CLASSIFICATION"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [
                 [org.clojure/clojure "1.5.1"]
                 [incanter "1.5.4"]]
  :main ^:skip-aot intro-ex.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
