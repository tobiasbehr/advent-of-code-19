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
