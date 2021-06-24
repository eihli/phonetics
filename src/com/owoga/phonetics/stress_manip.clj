(ns com.owoga.phonetics.stress-manip
  (:require [clojure.string :as string]))

(defn primary-stress?
  [phone]
  (re-find #"1" phone))

(defn non-primary-stress?
  [phone]
  (re-find #"[2-9]" phone))

(defn unstressed?
  [phone]
  (re-find #"0" phone))

(defn remove-any-stress-signifiers
  [phones]
  (map #(string/replace % #"\d" "") phones))

(defn remove-non-primary-stress-signifiers
  [phones]
  (map #(string/replace % #"[02-9]" "") phones))

(defn unify-stressed
  [phones]
  (map #(string/replace % #"[2-9]" "1") phones))

(def consonant-unification-map
  "This almost aligns with the phonemap that maps phones to whether they are vowels, aspirates, nasals, etc...
  Slight but possibly important difference in stops. For example, I think T and D
  are more unified than T and G; and G and K are more unifide than G and T."
  {"T" "T"
   "CH" "CH"
   "K" "K"
   "HH" "HH"
   "L" "L"
   "JH" "CH" ;; <-
   "G" "K"   ;; <-
   "M" "M"   ;; <-
   "S" "S"
   "Y" "Y"
   "Z" "S"   ;; <-
   "R" "R"
   "F" "F"
   "B" "B"
   "SH" "CH" ;; <-
   "P" "B"   ;; <-
   "V" "F"   ;; <-
   "TH" "T"  ;; <-
   "N" "M"   ;; <-
   "DH" "T"  ;; <-
   "W"  "Y"  ;; <-
   "ZH" "S"  ;; <-
   "NG" "M"  ;; <-
   "D" "T"   ;; <-
   })

(defn unify-consonants
  [phones]
  (mapv #(get consonant-unification-map % %) phones))

(defn remove-unstressed-signifiers
  [phones]
  (map #(string/replace % #"0" "")))
