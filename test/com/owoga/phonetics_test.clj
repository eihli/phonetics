(ns com.owoga.phonetics-test
  (:require [clojure.test :refer :all]
            [com.owoga.phonetics :refer :all]))

(deftest phonetics-test
  (testing "word to phones"
    (is (= [["HH" "AH0" "L" "OW1"]
            ["HH" "EH0" "L" "OW1"]]
           (get-phones "hello"))))
  (testing "phones to word"
    (is (= ["hello(1)"]
           (get-word ["HH" "EH0" "L" "OW1"])))
    (is (= ["hello(1)"]
           (get-word ["HH" "EH" "L" "OW"])))
    (is (= ["ensure(1)" "insure"]
           (get-word ["IH" "N" "SH" "UH" "R"])))))
