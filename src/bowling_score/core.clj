(ns bowling-score.core
  (:require [clojure.string :as string])
  (:gen-class))

(def max-pins 10)
(def max-turns 10)


(def blank-scorecard (vec (take max-turns (repeat [:skip :skip]))))
(def test-scorecard [[1 3] [2 5] [7 2] [:strike :skip] [3 :spare] [4 4] [2 2] [3 5] [2 2] [4 2]])

(defn pretty-bowl [bowl] (case bowl :strike \X, :spare \/, :skip \-, bowl))
(defn pretty-card [scorecard] (->> scorecard (clojure.walk/postwalk pretty-bowl) 
                                     (map #(string/join ", " %)) (string/join " | ")))


(defn raw-bowls 
  "Converts a scorecard into a sequence of raw pins-hit that make it up."
  [scorecard]
  (assert (vector? scorecard)) ; for expected conj, peek behaviour
  (->> scorecard flatten 
                  (filter #(not (= :skip %))) 
                  (reduce (fn [coll val] 
                              (conj coll 
                                 (case val :strike max-pins, 
                                           :spare (- max-pins (peek coll)), val)))
                   [])
  ))

(defn valid-played-frame? [frame & {:keys [bonus?] :or {bonus? false}}]
  (let [[ball1 ball2 & extras] frame
        pending (cond 
                  (and (every? #(and (number? %) (< % max-pins)) [ball1 ball2])
                    (< (+ ball1 ball2) max-pins))  0
                  (= [:strike :skip] [ball1 ball2]) 2
                  (and (number? ball1) (< ball1 max-pins) (= :spare ball2)) 1
             :else false)]
       (if bonus?
           (and (= (count extras) pending)
                (every? #(or (number? %) (#{:strike :spare} %)) extras)

                ; we don't really check here for valid scores, just decipherable ones
                (try (= (count (raw-bowls (vec extras))) pending) (catch Exception e false)))
           (and (= (count extras) 0) pending))
  ))

(defn finished-card? [scorecard] (and (= (count scorecard) max-turns)
                                   (every? valid-played-frame? (take (- max-turns 1) scorecard))
                                   (valid-played-frame? (last scorecard) :bonus true)))

(defn calculate-score [scorecard]
  (loop [frames scorecard, total 0]
    (if (not-empty frames) 
          (let [[b1 b2 :as frame] (first frames)
            context (raw-bowls frames)
            frame-scores (cond
                           (every? number? [b1 b2]) (take 2 context)
                           (= [b1 b2] [:strike :skip]) (take 3 context)
                           (and (number? b1) (= :spare b2)) (take 3 context))
            sum (apply + frame-scores)]
            (recur (subvec frames 1) (+ total sum)))
         total)))
         
        

(defn -main
  "Runs the bowling score input loop."
  [& args]
  (println (pretty-card test-scorecard)))
