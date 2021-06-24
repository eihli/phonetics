(ns com.owoga.phonetics
  (:require [clojure.set]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combinatorics])
  (:import (com.sun.speech.freetts.en.us CMULexicon)))

#_(set! *warn-on-reflection* true)

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

(def cmu-word-alternatives
  "For words with multiple pronunciations in the CMU dictionary,
  this maps from the word to its variations.
  reputed -> reputed, reputed(1), reputed(2).

  Not particularly useful itself since reputed(1) doesn't tell you how it's
  different from reputed. But it's useful to look up the pronunciations in the
  CMU dictionary."
  (reduce
   (fn [m k]
     (let [norm-key (string/replace k #"\(\d\)" "")]
       (update m norm-key (fnil (comp sort conj) []) k)))
   {}
   (keys cmu-word-to-stressed-phones-map)))

(defn word-alternatives
  "For words with multiple pronunciations in the CMU dictionary,
  this maps from the word to its variations.
  reputed -> reputed, reputed(1), reputed(2).

  Not particularly useful itself since reputed(1) doesn't tell you how it's
  different from reputed. But it's useful to look up the pronunciations in the
  CMU dictionary."
  [word]
  (get cmu-word-alternatives word))

(def stressed-phones-to-cmu-word-map
  "The same sequence of phones can map to multiple words."
  (reduce
   (fn [m [k v]]
     (update m v (fnil conj []) k))
   {}
   cmu-word-to-stressed-phones-map))

(def cmu-word-to-unstressed-phones-map
  (->> cmu-word-to-stressed-phones-map
       (mapv (fn [[k v]] [k (mapv #(string/replace % #"\d" "") v)]))
       (into {})))

(def unstressed-phones-to-cmu-word-map
  "There might be unstressed phones that can map
  to two different pronunciations when stress is added,
  so this maps unstressed phones to a vector of words that
  can be looked up in the CMU Pronouncing dictionary to
  see what their stressed phones are.

  Another example, look at how many words map to [N IY S].
  [[N IY S]
  [neice neece niece nice kneece kniess neiss neace niess]]"
  (reduce
   (fn [m [k v]]
     (let [v (map #(string/replace % #"\d" "") v)]
      (update m v (fnil conj []) k)))
   {}
   cmu-word-to-stressed-phones-map))

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
  (.indexOf sonority-hierarchy (phonemap phone)))

(defn vowel? [phone]
  (vowel (string/replace phone #"\d" "")))

(def consonant? (complement vowel?))

(defn >sonorous [a b]
  (< (sonority a) (sonority b)))

(defn <sonorous [a b]
  (> (sonority a) (sonority b)))

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
          ((fn [^String phoneme]
             (if (.equals phoneme "ax")
               "ah"
               phoneme)))
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

  Input must be lower-case.

  Returns a vector of all possible pronunciations."
  [word]
  (let [cmu-phones (mapv cmu-word-to-stressed-phones-map (word-alternatives word))]
    (if (seq cmu-phones)
      cmu-phones
      [(cmu-lexicon->cmu-pronouncing-dict
        (.getPhones cmu-lexicon word nil))])))

(defn get-word
  "Returns vector of all words that are in the CMU pronouncing dictionary
  that have the pronunciation given `phones`.

  Expects phones to have stress removed.

  Not an exact inverse of `get-phones` since `get-phones` can figure out
  somewhat appropriate phones for a made-up word. This function cannot
  figure out the spelling of a made-up word provided the made-up word's phones.

  Returns nil if no word can be found."
  [phones]
  (let [stressed? (some #(re-matches #".*\d" %) phones)]
    (if stressed?
      (stressed-phones-to-cmu-word-map phones)
      (unstressed-phones-to-cmu-word-map phones))))

(defn phrase-phones
  "Pronunciations of a words seperated by spaces."
  [phrase]
  (->> phrase
       (#(string/split % #" "))
       (map get-phones)
       (apply combinatorics/cartesian-product)
       (mapv (partial reduce into []))))

(comment
  (get-phones "alaska")
  ;; => [["AH0" "L" "AE1" "S" "K" "AH0"]]
  (syllabify (first (get-phones "alaska")))
  ;; => [["AH0"] ["L" "AE1" "S"] ["K" "AH0"]]
  (syllabify (first (get-phones "foobarbazia")))
  ;; => [["F" "UW1"] ["B" "AA1" "R"] ["B" "AA1"] ["Z" "IY0"] ["AH0"]]

  (get-word ["AH" "L" "AE" "S" "K" "AH"])
  ;; => ["alaska"]
  (get-word ["N" "IY" "S"])
  ;; => ["neice" "neece" "niece" "nice(1)" "kneece" "kniess" "neiss" "neace" "niess"]
  (get-word ["F" "UW" "B" "AE" "Z"])
  ;; => nil
  (phrase-phones "bog hog")
  ;;  [["B" "AA1" "G" "HH" "AA1" "G"]
  ;;   ["B" "AO1" "G" "HH" "AA1" "G"]]

  )
