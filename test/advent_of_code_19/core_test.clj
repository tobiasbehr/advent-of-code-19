(ns advent-of-code-19.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code-19.core :refer :all]))

(deftest day1-1-test
  (testing "Fuel for mass"
    (is (= 2 (fuel-for-mass 12)))
    (is (= 2 (fuel-for-mass 14)))
    (is (= 654 (fuel-for-mass 1969)))
    (is (= 33583 (fuel-for-mass 100756)))))

(deftest day1-2-test
  (testing "Fuel for module"
    (is (= 2 (fuel-for-module 14)))
    (is (= 966 (fuel-for-module 1969)))
    (is (= 50346 (fuel-for-module 100756)))))


(deftest calculate-window-test
  (testing "calculate single window"
    (is (= [2 0 0 0 99] (calculate-window [1 0 0 0] [1 0 0 0 99])))
    (is (= [2 3 0 6 99] (calculate-window [2 3 0 3] [2 3 0 3 99])))
    (is (= [2 4 4 5 99 9801] (calculate-window [2 4 4 5] [2 4 4 5 99 0])))))


(deftest calculator-test
  (testing "calculation"
    (is (= [2 0 0 0 99] (calculator [1 0 0 0 99] 0)))))

(deftest day2-1-test
  (testing "calculation"
    (is (= [30 1 1 4 2 5 6 0 99] (calculate [1 1 1 4 99 5 6 0 99])))
    (is (= [30 1 1 4 2 5 6 0 99 1 2 2 3] (calculate [1 1 1 4 99 5 6 0 99 1 2 2 3])))))


(deftest next-point-test
  (testing "calculation of next point"
    (is (= [1 0] (next-point [0 0] ["R" 1])))
    (is (= [-1 0] (next-point [0 0] ["L" 1])))
    (is (= [0 1] (next-point [0 0] ["U" 1])))
    (is (= [0 -1] (next-point [0 0] ["D" 1])))))

(deftest next-points-test
  (testing "calculation of next points"
    (is (= [[1 0] [2 0] [3 0]] (next-points [0 0] ["R" 3])))
    (is (= [[1 0] [2 0]] (next-points [0 0] ["R" 2])))
    (is (= [[-1 0] [-2 0] [-3 0]] (next-points [0 0] ["L" 3])))
    (is (= [[0 -1] [0 -2] [0 -3]] (next-points [0 0] ["D" 3])))
    (is (= [[0 1] [0 2] [0 3]] (next-points [0 0] ["U" 3])))))

(deftest parse-direction-test
  (testing "parsing directions"
    (is (= ["R" 1] (parse-direction "R1")))
    (is (= ["R" 12] (parse-direction "R12")))))

(deftest next-locations-test
  (testing ""
    (is (= [[0 0] [1 0] [2 0]] (route ["R2"])))))
