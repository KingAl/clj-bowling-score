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

(defn empty-frame? [frame]
  (and (= (count frame) 2) (every? #{:skip} frame)))

(defn played-pair?
  "Takes initial 2 scores in a frame, if playable returns number of pending
   rolls required to calculate the frame score"
  [frame-pair]
  (let [[b1 b2] frame-pair
        pending (cond
                  (and (every? #(and (number? %) (< % max-pins)) [b1 b2])
                    (< (+ b1 b2) max-pins))  0
                  (= [:strike :skip] [b1 b2]) 2
                  (and (number? b1) (< b1 max-pins) (= :spare b2)) 1
             :else false)]
    pending))

(defn valid-bonus? [bonus pending]
  (let [[b1 b2] bonus]
    (and (= (count bonus) pending)
      (case pending
            0 true
            1 (or (= b1 :strike) (and (number? b1) (< b1 max-pins)))
            2 (or (and (every? number? bonus) (< (+ b1 b2) max-pins))
                  (every? #{:strike} bonus)
                  (and (number? b1) (< b1 max-pins) (= :spare b2)))))))

(defn valid-played-frame? [frame & {:keys [bonus?] :or {bonus? false}}]
  (let [[b1 b2 & extras] frame
        pending (played-pair? [b1 b2])]
       (if bonus?
           (valid-bonus? extras pending)
           (and (= (count extras) 0) pending))
  ))

(defn finished-card? [scorecard] (and (= (count scorecard) max-turns)
                                   (every? valid-played-frame? (take (- max-turns 1) scorecard))
                                   (valid-played-frame? (last scorecard) :bonus? true)))

(defn calculate-score [scorecard & {:keys [reducef reducev] :or {reducef + reducev 0}}]
  (loop [frames scorecard, total reducev]
    (if (not-empty frames) 
          (let [[b1 b2 :as frame] (first frames)
            context (raw-bowls frames)
            frame-scores (cond
                           (every? number? [b1 b2]) (take 2 context)
                           (= [b1 b2] [:strike :skip]) (take 3 context)
                           (and (number? b1) (= :spare b2)) (take 3 context))
            sum (apply + frame-scores)]
            (recur (subvec frames 1) (reducef total sum)))
         total)))

(defn per-frame-score [scorecard]
  (calculate-score scorecard :reducef conj :reducev []))

(defn cumul-score [scorecard]
  (reductions + (per-frame-score scorecard)))

        
(defn -main
  "Runs the bowling score input loop."
  [& args]
  (println (pretty-card test-scorecard)))
