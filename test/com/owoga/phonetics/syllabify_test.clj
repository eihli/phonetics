(ns com.owoga.phonetics.syllabify-test
  (:require [clojure.test :refer :all]
            [com.owoga.phonetics.syllabify :refer :all]))

(deftest syllabification-test
  (testing "alaska"
    (is (= '(("AH") ("L" "AE" "S") ("K" "AH"))
           (syllabify '("AH" "L" "AE" "S" "K" "AH")))))
  (testing "parentheses"
    (is (= '(("P" "ER") ("IH" "N") ("TH" "UH") ("S" "IY" "S"))
           (syllabify '("P" "ER" "IH" "N" "TH" "UH" "S" "IY" "S")))))
  (testing "herald"
    (is (= '(("H" "ER") ("AH" "L" "D"))
           (syllabify '("H" "ER" "AH" "L" "D")))))
  (testing "royal with cheese"
    (is (= '(("R" "OY") ("AH" "L") ("W" "IH" "TH") ("CH" "IY" "Z"))
           (syllabify ["R" "OY" "AH" "L" "W" "IH" "TH" "CH" "IY" "Z"]))))
  (testing "uprising"
    (is (= '(("UH" "P") ("R" "AY") ("S" "IY" "NG"))
           (syllabify ["UH" "P" "R" "AY" "S" "IY" "NG"]))))
  (testing "glimpstred"
    (is (= '(("G" "L" "IH" "M" "P" "S") ("T" "R" "EH" "D"))
           (syllabify ["G" "L" "IH" "M" "P" "S" "T" "R" "EH" "D"]))))
  (testing "boink"
    (is (= '(("B" "OY" "N" "K"))
           (syllabify ["B" "OY" "N" "K"]))))
  ;; Lax vowels can only occur in closed syllables.
  (testing "elipsis"
    (is (= '(("IY") ("L" "IH" "P") ("S" "IH" "S"))
           (syllabify ["IY" "L" "IH" "P" "S" "IH" "S"]))))
  ;; http://www.glottopedia.org/index.php/Maximal_Onset_Principle
  (testing "maximal onset principle"
    (testing "diploma"
      (is (= '(("D" "IH" "P") ("L" "OW") ("M" "AH"))
             (syllabify ["D" "IH" "P" "L" "OW" "M" "AH"])))))
  ;; http://www.glottopedia.org/index.php/Ambisyllabic
  ;; Since we are syllabifying phones, we don't need to worry
  ;; about handling ambisyllabic words. There's no such thing.
  (testing "pillow"
    (is (= '(("P" "IH") ("L" "OW"))
           (syllabify ["P" "IH" "L" "OW"]))))
  (testing "steel"
    (is (= [["S" "T" "IY1" "L"]]
           (syllabify ["S" "T" "IY1" "L"]))))
  (testing "scotch"
    (is (= [["S" "K" "AA1" "CH"]]
           (syllabify ["S" "K" "AA1" "CH"])))))
