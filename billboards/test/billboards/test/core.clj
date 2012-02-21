(ns billboards.test.core
  (:use [billboards.core])
  (:use [clojure.test]))

(deftest take-line-test
  (testing "Empty line"
    (is (= [[] nil] (take-line 10 []))))
  (testing "Just one line"
    (is (= [["Very" "long"] nil] (take-line 10 ["Very" "long"]))))
  (testing "A line"
    (is (= [["Very" "long"] ["line"]] (take-line 10 ["Very" "long" "line"])))))

(deftest make-paragraph-test
  (testing "One line paragraph"
    (is (= [["One" "line" "paragraph."]]
          (make-para 80 ["One" "line" "paragraph."]))))
  (testing "Empty paragraph"
    (is (= [] (make-para 80 []))))
  (testing "Two lines"
    (is (= [["Very" "long"]
            ["text."]] (make-para 10 ["Very" "long" "text."])))))

(deftest max-font-test
  (testing "Doesn't fit"
    (is (= nil (max-font 10 1 "Very long text."))))
    (is (= 3 (max-font 20 6 "hacker cup"))))

(deftest main-test
  (testing "Sample input"
    (is (= (str
             "Case #1: 3\n"
             "Case #2: 10\n"
             "Case #3: 2\n"
             "Case #4: 8\n"
             "Case #5: 7\n")
          (with-out-str (-main "example.txt"))))))