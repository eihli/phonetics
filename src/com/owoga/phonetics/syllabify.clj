(ns com.owoga.phonetics.syllabify
  (:require [com.owoga.phonetics :as phonetics]
            [com.owoga.phonetics.util :as util]
            [clojure.string :as string]))

#_(set! *warn-on-reflection* true)

(defn slurp-rime
  "Expects the phones in reverse order.
  Returns a vector of the rime (in forwards order) and the remaining phones to process."
  [phones]
  (let [splits (util/take-through phonetics/vowel? phones)]
    [(vec (reverse (first splits))) (vec (flatten (rest splits)))]))

(comment
  (slurp-rime ["AH" "K" "S" "AE" "L" "AH"])
  ;; => [["AH"] ["K" "S" "AE" "L" "AH"]]
  (slurp-rime ["K" "S" "AE" "L" "AH"])
  ;; => [["AE" "S" "K"] ["L" "AH"]]
  )

(defn slurp-onset-given-rime
  "Phones and rime are vectors of phones.
  Phones is backwards since we process naturally that way
  due to the maximal onset principle. Rime is forwards since
  it's the end-result of how we're reading the word.

  Returns a vector of the syllable and the remaining phones to process."
  [phones rime]
  (loop [phones phones
         syllable rime]
    (cond
      (empty? phones) [syllable phones]

      ;; Two vowels next to each other is treated as two syllables.
      ;; This might not always be the case if the vowels are lax.
      ;; Is "royal" 1 syllable or two? This treats it as two.
      (phonetics/vowel? (nth phones 0))
      [syllable phones]

      ;; Maximal onset principle with exception for lax vowels occurring in
      ;; closed syllables.
      (and (phonetics/consonant? (nth syllable 0))
           (phonetics/<sonorous (nth phones 0) (nth syllable 0))
           (not (phonetics/lax-vowels (nth phones 1 nil))))
      (recur (subvec phones 1)
             (into [(nth phones 0)] syllable))

      (phonetics/vowel? (nth syllable 0))
      (recur (subvec phones 1)
             (into [(nth phones 0)] syllable))

      (not-any? phonetics/vowel? phones)
      (recur (subvec phones 1)
             (into [(nth phones 0)] syllable))

      :else [syllable phones])))

(comment
  (slurp-onset-given-rime
   ["K" "S" "AE" "L" "A"]
   ["AH"])

  )
(defn apply-stress [unstressed-syllables stressed-phones]
  (loop [unstressed-syllables unstressed-syllables
         stressed-phones stressed-phones
         result-syllables [[]]]
    (cond
      (empty? stressed-phones)
      result-syllables

      (empty? (first unstressed-syllables))
      (recur (rest unstressed-syllables)
             stressed-phones
             (conj result-syllables []))

      :else
      (recur
       (cons (rest (first unstressed-syllables))
             (rest unstressed-syllables))
       (rest stressed-phones)
       (conj (pop result-syllables) (conj (peek result-syllables) (first stressed-phones)))))))

(comment
  (apply-stress '(("AH") ("L" "AE" "S") ("K" "AH"))
                '("AH0" "L" "AE1" "S" "K" "AH0"))
  ;; => [["AH0"] ["L" "AE1" "S"] ["K" "AH0"]]

  )

(defn syllabify [original-phones]
  ;; It's easier to work backwards.
  ;; The final syllable will always be
  ;; all of the last (if any) consonants preceded by
  ;; (or folllowed-by considering we're working
  ;; backwards through the phones) a vowel.
  ;; So, reverse the phones as a first step.
  (let [phones (phonetics/remove-stress (reverse original-phones))]
    (loop [phones phones
           segments []]
      (if (empty? phones)
        (apply-stress segments original-phones)
        (let [[rime phones'] (slurp-rime phones)
              [syllable phones''] (slurp-onset-given-rime phones' rime)]
          (recur phones'' (into [syllable] segments)))))))

(comment
  (syllabify ["S" "T" "IY" "L"])
  (slurp-rime (reverse ["S" "T" "IY" "L"]))
  (slurp-onset-given-rime ["T" "S"] ["IY" "L"])
  (phonetics/remove-stress ["AH" "L" "AE" "S" "K" "AH"])
  (slurp-onset-given-rime ["L" "AE" "S" "K" "AH"] ["AH"])
  (syllabify ["AH0" "L" "AE1" "S" "K" "AH0"])

  )
