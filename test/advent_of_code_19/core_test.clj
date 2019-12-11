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


(deftest distance-test
  (testing "determination of distance of a certain point in a route"
    (is (= 3 (distance [[0 0] [0 1] [1 1] [1 2]] [1 2])))))


;;; Day 4

(deftest digits-not-decreasing?-test
  (testing "digits are not decreasing"
    (is (true? (digits-not-decreasing? 1234)))
    (is (true? (digits-not-decreasing? 12223)))
    (is (not (true? (digits-not-decreasing? 12221))))))

(deftest has-same-adjacent-digits?-test
  (testing "at least on pair of adjacent digits is equal"
    (is (true? (has-same-adjacent-digits? 1223)))
    (is (true? (not (has-same-adjacent-digits? 1234))))))

(deftest password?-test
  (testing "input string meets password criteria"
    (is (not (true? (password? "hallo"))))
    (is (not (true? (password? 123))))
    (is (true? (password? 123456)))
    (is (not (true? (password? 1234567))))))

(deftest handle-greater-test
  (testing "handling if a greater digit in the number"
    (is (= true (:matches (handle-greater {:count 2 :position 3 :matches false} 3))))
    (is (= false (:matches (handle-greater {:count 3 :position 3 :matches false} 3))))))

(deftest two-matching-adjacent-digits?-test
  (testing "digits are not decreasing"
    (is (not (true? (two-matching-adjacent-digits? 123456))))
    (is (true? (two-matching-adjacent-digits? 122345)))
    (is (true? (two-matching-adjacent-digits? 123455)))
    (is (not (true? (two-matching-adjacent-digits? 122234))))
    (is (not (true? (two-matching-adjacent-digits? 122212))))))

;;; Day 5

(deftest get-value-test
  (testing "retrieval of values in position and immediate mode"
    (is (= 2 (get-value 1 2 [1 2 3 4])))
    (is (= 3 (get-value 0 2 [1 2 3 4])))))

(deftest modes-test
  (testing "retrieval of values in position and immediate mode"
    (is (= [0 1 0] (modes 1001)))))

(deftest determine-op-test
  (testing "determination of operation :io or :calculation"
    (is (= :io (determine-op {:memory [3 2 3 4 99] :pointer 0})))
    (is (= :io (determine-op {:memory [4 2 3 4 99] :pointer 0})))
    (is (= :calculation (determine-op {:memory [1 2 3 4 99] :pointer 0})))
    (is (= :calculation (determine-op {:memory [2 2 3 4 99] :pointer 0})))))

(deftest execute-instruction-test
  (testing ""
    (is (= {:memory [1002 4 3 4 99] :pointer 4} (execute-instruction {:memory [1002 4 3 4 33] :pointer 0})))
    (is (= {:memory [1 4 3 4 37] :pointer 4} (execute-instruction {:memory [1 4 3 4 33] :pointer 0})))
    (is (= {:memory [1101 4 3 4 7] :pointer 4} (execute-instruction {:memory [1101 4 3 4 33] :pointer 0})))))

(deftest execute-program-test
  (testing "execution of a program"
    (is (= [1 4 2 2 2 2 2 1 99] (execute-program {:memory [1 0 0 2 2 2 2 1 99]})))))
