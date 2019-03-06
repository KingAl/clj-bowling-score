(ns bowling-score.core
  (:require [clojure.string :as string])
  (:gen-class))

(def test-scorecard [[1 3] [2 5] [7 2] [:strike :skip] [3 :spare] [4 4] [2 2] [3 5] [2 2] [4 2]])

(defn pretty-bowl [bowl] (case bowl :strike \X, :spare \/, :skip \-, bowl))
(defn pretty-card [scorecard] (->> scorecard (clojure.walk/postwalk pretty-bowl) 
                                     (map #(string/join ", " %)) (string/join " | ")))


(defn raw-bowls [scorecard]
   (assert (vector? scorecard)) ; for expected conj, peek behaviour
   (->> scorecard flatten 
                  (filter #(not (= :skip %))) 
                  (reduce (fn [coll val] 
                              (conj coll 
                                 (case val :strike 10, 
                                           :spare (- 10 (peek coll)), val)))
                   [])
   ))


(defn -main
  "Runs the bowling score input loop."
  [& args]
  (println (pretty-card test-scorecard)))
