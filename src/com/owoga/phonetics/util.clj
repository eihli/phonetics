(ns com.owoga.phonetics.util)

#_(set! *warn-on-reflection* true)

(defn take-through
  "(take-through even? [1 2 3 4 7 7 5 2 8 10])
   returns '((1 2 3 4) (7 7 5 2) (8) (10))"
  [pred coll]
  (loop [coll coll
         acc '()]
    (cond
      (empty? coll)
      (if (empty? acc) acc (list (reverse acc)))

      (pred (first coll))
      (let [acc (cons (first coll) acc)]
        (lazy-seq (cons (reverse acc) (take-through pred (rest coll)))))

      :else
      (recur (rest coll)
             (cons (first coll) acc)))))
