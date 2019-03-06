(ns bowling-score.core-test
  (:require [clojure.test :refer :all]
            [bowling-score.core :refer :all]))

(deftest print-score-test
  (testing "Basic score display test"
    (is (= (pretty-card [[1 2]]) "1, 2"))
    (is (= (pretty-card [[2 3] [4 5]]) "2, 3 | 4, 5"))
    (is (= (pretty-card [[2 3] [:strike :skip]]) "2, 3 | X, -"))
    (is (= (pretty-card [[1 :spare]]) "1, /"))
))

(deftest raw-bowls-test
  (testing "Converting a scorecard to a sequence of raw pins knocked down"
    (is (= (raw-bowls [[1 2]]) [1 2]))
    (is (= (raw-bowls [[2 3] [4 5]]) [2 3 4 5]))
    (is (= (raw-bowls [[2 3] [:strike :skip]]) [2 3 10]))
    (is (= (raw-bowls [[1 :spare]]) [1 9]))
))

(deftest calc-score-test
  (testing "Calculating total score from a scorecard"
    (is (= (calculate-score [[1 2]]) 3))
    (is (= (calculate-score [[2 3] [4 5]]) 14))
    (is (= (calculate-score [[2 3] [:strike :skip] [2 2]]) 23))
    (is (= (calculate-score [[1 :spare] [2 2]]) 16))
))
