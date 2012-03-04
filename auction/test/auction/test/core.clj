(ns auction.test.core
  (:use [auction.core])
  (:use [clojure.test]))

(deftest product-generation
  (testing "Random number generation"
    (is (= [0 6 3 1 2]
           (take 5 (random-seq 0 3 5 7)))))
    (is (= [[0 0] [6 5] [3 5] [1 5] [2 5]]
           (take 5 (product-seq 0 0 7 5 3 5 2 4)))))

(deftest partial-ordering
  (testing "vector ordering"
    (is (= true  (better? [0 1] [1 2])))
    (is (= true  (better? [0 1] [1 1])))
    (is (= true  (better? [0 1] [0 2])))
    (is (= false (better? [0 1] [0 1])))
    (is (= false (better? [0 1] [0 0]))))
  (testing "preferred accumulation"
    (is (= [[0 0]]       (bests [] [0 0])))         
    (is (= [[0 0] [0 0]] (bests [[0 0]] [0 0])))         
    (is (= [[0 0]]       (bests [[0 1]] [0 0])))
    (is (= [[0 1] [1 0]] (bests [[0 1]] [1 0])))
    (is (= [[0 0]]       (bests [[0 1] [1 0]] [0 0])))))

(deftest auctioning
  (is (= [3 3] (auction 5 1 4 5 7 1 0 1 2)))
  (is (= [3 3] (auction 3 1 3 3 3 1 0 1 1)))
  (is (= [2 3] (auction 8 1 3 3 3 1 0 1 2)))
  (is (= [2 2] (auction 13 5 7 5 9 1 3 2 5)))
  (is (= [3 1] (auction 11 2 3 5 7 11 13 17 19))))

(deftest text-user-interface
  (testing "Input parsing"
    (is (= [[1 2 3 4 5 6 7 8 9]] (parse-input ["1" "1 2 3 4 5 6 7 8 9"]))))
  (testing "Output formatting"
    (is (= "Case #3: 1 2\n" (with-out-str (print-case 2 [1 2]))))))
