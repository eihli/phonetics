(ns com.owoga.phonetics
  (:require [clojure.set]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.set :as set])
  (:import (com.sun.speech.freetts.en.us CMULexicon)))

;; From http://svn.code.sf.net/p/cmusphinx/code/trunk/cmudict/cmudict-0.7b.phones

(def phonemap
  {"T"  "stop",
   "CH" "affricate",
   "K"  "stop",
   "HH" "aspirate",
   "UH" "vowel",
   "AY" "vowel",
   "AH" "vowel",
   "OW" "vowel",
   "L"  "liquid",
   "JH" "affricate",
   "UW" "vowel",
   "G"  "stop",
   "EH" "vowel",
   "M"  "nasal",
   "OY" "vowel",
   "S"  "fricative",
   "Y"  "semivowel",
   "EY" "vowel",
   "Z"  "fricative",
   "R"  "liquid",
   "F"  "fricative",
   "AW" "vowel",
   "IY" "vowel",
   "B"  "stop",
   "SH" "fricative",
   "P"  "stop",
   "V"  "fricative",
   "TH" "fricative",
   "IH" "vowel",
   "AA" "vowel",
   "AO" "vowel",
   "N"  "nasal",
   "DH" "fricative",
   "W"  "semivowel",
   "ZH" "fricative",
   "NG" "nasal",
   "D"  "stop",
   "ER" "vowel",
   "AE" "vowel"})

(def long-vowel #{"EY" "IY" "AY" "OW" "UW"})

(def short-vowel #{"AA" "AE" "AH" "AO" "AW" "EH" "ER" "IH" "OY" "UH"})

(def vowel (clojure.set/union long-vowel short-vowel))

(def consonant (clojure.set/difference (into #{} (keys phonemap)) vowel))

(def syllable-end (clojure.set/union consonant long-vowel))

(def single-sound-bigram #{"TH" "SH" "PH" "WH" "CH"})

(def cmu-word-to-stressed-phones-map
  "Map of lowercase English words to their phonetic sounding based on
  the CMU Pronouncing Dictionary at http://www.speech.cs.cmu.edu/cgi-bin/cmudict/

  Includes words with apostrophes, like possessive aaronson's.

  Words with multiple pronunciations have keys with a `(1)` or `(2)` after their
  duplicates, like [aaronsons(1) (AA1 R AH0 N S AH0 N Z)]

  Primary stress is indicated by a `1` after the phoneme. Secondary stress with a `2`.
  Unstressed with a `0`."
  (->> "cmudict-0.7b"
       io/resource
       io/reader
       line-seq
       (drop-while #(= \; (first %)))
       (map #(string/split % #"\s+"))
       (map (partial split-at 1))
       (map #(vector
              (string/lower-case
               (first (first %)))
              (vec (second %))))
       (into {})))

(def stressed-phones-to-cmu-word-map
  (set/map-invert cmu-word-to-stressed-phones-map))

(def cmu-word-to-unstressed-phones-map
  "For words with multiple pronunciations with different stresses,
  drop/ignore all but the first. The choice to do so is arbitrary.
  An alternative would be a map where two different keys mapped to
  the same value."
  (->> cmu-word-to-stressed-phones-map
       (remove (fn [[k _]] (re-matches #".*\(\d\)" k)))
       (map (fn [[k v]] [k (map #(string/replace % #"\d" "") v)]))))

(def unstressed-phones-to-cmu-word-map
  (set/map-invert cmu-word-to-unstressed-phones-map))

(CMULexicon. "cmulex" true)

(def ^CMULexicon cmu-lexicon
  "The CMULexicon can get phones for words that aren't in the
  CMU Pronouncing Dictionary. But the phones are slightly different.
  The `AH` sound, as in `allow`, is returned as `ax` from the CMULexicon.
  Also, unstressed vowels don't have a `0` suffix. Instead, the CMULexicon
  just returns unstressed vowels as the vowel itself with no suffix.

  The above is important to note if you want clean interplay between these
  two different ways of getting phonemes."
  (CMULexicon/getInstance true))

(defn remove-stress [phonemes]
  (mapv #(string/replace % #"\d" "") phonemes))

(defn cmu-lexicon->cmu-pronouncing-dict
  "The CMULexicon returns the `AH` sound, as in `allow`, as `ax`.
  The Sphinx dictionary treates that sound as `AH`. This
  converts `ax` to `AH`. It also adds `0` to phonemes that are
  unstressed, which CMULexicon returns as the plain phoneme with
  no stress marker."
  [phonemes]
  (mapv
   (fn [phoneme]
     (->> phoneme
          (#(if (.equals % "ax") "ah" %))
          string/upper-case
          (#(if (vowel %) (str % "0") %))))
   phonemes))

(comment
  (type (.getPhones cmu-lexicon "allow" nil)) ;; => [Ljava.lang.String;
  (vec (.getPhones cmu-lexicon "allow" nil))  ;; => ["ax" "l" "aw1"]
  (cmu-lexicon->cmu-pronouncing-dict
   (.getPhones cmu-lexicon "allowance" nil))
  ;; => ["AH0" "L" "AW1" "AH0" "N" "S"]
  (cmu-word-to-stressed-phones-map "allowance")
  ;; => ["AH0" "L" "AW1" "AH0" "N" "S"]
  )

(defn get-phones
  "Tries to get phones first from the CMU Pronouncing Dictionary
  and falls back to the CMULexicon if the word doesn't exist in
  the dictionary.

  Input must be lower-case."
  [word]
  (or (cmu-word-to-stressed-phones-map word)
      (cmu-lexicon->cmu-pronouncing-dict
       (.getPhones cmu-lexicon word nil))))

(comment
  ;; This demonstrates a problem with how we're handling words with multiple
  ;; pronunciations.
  (get-phones "hello")
  ;; => ["HH" "AH0" "L" "OW1"]
  ;; I pronounce that ["HH" "EH0" "L" "OW1"]
  )
