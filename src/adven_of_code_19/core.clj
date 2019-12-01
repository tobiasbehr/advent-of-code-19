(ns adven-of-code-19.core)

(def input1 (map #(Integer. %) (clojure.string/split (slurp "resources/input1.txt") #"\n")))
(def input1-test [1969]) ; result 654

(defn fuel-for-mass [mass]
  (- (Math/floor (/ mass 3)) 2))

(defn fuel-for-module [mass]
  (let [fuel (fuel-for-mass mass)]
    (if (> fuel 0)
      (+ fuel (fuel-for-module fuel))
      0)))

(defn day1-1 [input]
  (->> input
       (map fuel-for-mass)
       (reduce +)))

(defn day1-2 [input]
  (->> input
       (map fuel-for-module)
       (reduce +)))
