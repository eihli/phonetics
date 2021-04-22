(ns com.owoga.phonetics.syllabify
  (:require [com.owoga.phonetics :as phonetics]
            [com.owoga.phonetics.util :as util]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

;; This sonority hierarchy may not be perfect.
;; It stems from: http://www.glottopedia.org/index.php/Sonority_hierarchy
;; I tried to match the phones provided by the CMU dict to the hierarchies
;; listed on that page:
;;   vowels > liquids > nasals > voiced fricatives
;;   > voiceless fricatives = voiced plosives
;;   > voiceless plosives (Anderson & Ewen 1987)
(def ^clojure.lang.PersistentVector sonority-hierarchy
  ;;   more sonorous  < < < vowel < < < (maximal onset) vowel > > > less sonorous
  ["vowel" "liquid" "semivowel" "aspirate" "affricate" "nasal" "fricative" "stop"])

(def lax-vowels #{"EH" "IH" "AE" "AH" "UH"})

(defn sonority [phone]
  (.indexOf sonority-hierarchy (phonetics/phonemap phone)))

(defn vowel? [phone]
  (phonetics/vowel phone))

(def consonant? (complement vowel?))

(defn >sonorous [a b]
  (< (sonority a) (sonority b)))

(defn <sonorous [a b]
  (> (sonority a) (sonority b)))

(defn slurp-rime [phones]
  (let [splits (util/take-through vowel? phones)]
    [(vec (reverse (first splits))) (vec (flatten (rest splits)))]))

(defn slurp-onset-given-rime
  "Phones and rime are vectors of phones.
  Phones is backwards since we process naturally that way
  due to the maximal onset principle. Rime is forwards since
  it's the end-result of how we're reading the word.

  Returns the unconsumed phones and the complete syllable."
  [phones rime]
  (loop [phones phones
         syllable rime]
    (cond
      (empty? phones) [phones syllable]

      ;; Two vowels next to each other is treated as two syllables.
      ;; This might not always be the case if the vowels are lax.
      ;; Is "royal" 1 syllable or two? This treats it as two.
      (vowel? (nth phones 0))
      [phones syllable]

      ;; Maximal onset principle with exception for lax vowels occurring in
      ;; closed syllables.
      (and (consonant? (nth syllable 0))
           (<sonorous (nth phones 0) (nth syllable 0))
           (not (lax-vowels (nth phones 1 nil))))
      (recur (subvec phones 1)
             (into [(nth phones 0)] syllable))

      (vowel? (nth syllable 0))
      (recur (subvec phones 1)
             (into [(nth phones 0)] syllable))

      :else [phones syllable])))

(defn syllabify [phones]
  ;; It's easier to work backwards.
  ;; The final syllable will always be
  ;; all of the last (if any) consonants preceded by
  ;; (or folllowed-by considering we're working
  ;; backwards through the phones) a vowel.
  ;; So, reverse the phones as a first step.
  (let [phones (vec (reverse phones))]
    (loop [phones phones
           segments []]
      (if (empty? phones)
        (map seq segments)
        (let [[rime phones] (slurp-rime phones)
              [phones syllable] (slurp-onset-given-rime phones rime)]
          (recur phones (into [syllable] segments)))))))
