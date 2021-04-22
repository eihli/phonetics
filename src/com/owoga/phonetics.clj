(ns com.owoga.phonetics
  (:require [clojure.set]))

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
