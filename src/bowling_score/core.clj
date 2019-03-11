(ns bowling-score.core
  (:require [clojure.string :as string])
  (:gen-class))

(def max-pins 10)
(def max-turns 10)


(def blank-scorecard (vec (take max-turns (repeat [:skip :skip]))))
(def test-scorecard [[1 3] [2 5] [7 2] [:strike :skip] [3 :spare] [4 4] [2 2] [3 5] [2 2] [4 2]])

(defn pretty-bowl [bowl] (case bowl :strike \X, :spare \/, :skip \-, bowl))
(defn pretty-card [scorecard] (->> scorecard (clojure.walk/postwalk pretty-bowl) 
                                     (map #(string/join ", " %)) (string/join " | ") (format " %s ")))
(defn pretty-score [frame-scores] (string/join "|" (map #(format "  %-4s" %) frame-scores)))


(defn raw-bowls 
  "Converts a scorecard into a sequence of raw pins-hit that make it up."
  [scorecard]
  (assert (vector? scorecard)) ; for expected end-item conj, peek behaviour
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
  "Takes initial 2 scores in a frame, if legal returns number of pending
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
                  (and (= b1 :strike) (number? b2) (< b2 max-pins))
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

(defn valid-card? 
  "Returns truthy if card is in legal state, :finished if game done, :incomplete if
  more frames are needed to make the score unambiguous."
  [scorecard]
  (if (empty-frame? (last scorecard))
    (let [played (take-while valid-played-frame? scorecard)
           pending-rolls (if (not-empty played) (valid-played-frame? (last played)) 0)
           unplayed (take-while empty-frame? (drop (count played) scorecard))]
         (and (= max-turns (+ (count played) (count unplayed)))
              (or (= pending-rolls 0) :incomplete)))
    (and (finished-card? scorecard) :finished)))


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

(defn score-game [scorecard]
  (if (finished-card? scorecard)
    (calculate-score scorecard)
    :unfinished))

(defn read-roll [prompt]
  (print prompt)
  (flush)
  (let [result 
          (try
            (let [b (Integer/parseInt (read-line))]
              (if (< 0 b (inc max-pins))
                b
                (do (println (format "Invalid roll, must be between 1 and %d." max-pins))
                 nil)))
           (catch NumberFormatException e (do (println "Invalid input, try again.") nil)))]
     (if (not result)
       (recur prompt)
       result)))


(defn play-round [round]
  (println "Round " round)
  (flush)
  (let [frame (let [b1 (read-roll "Ball 1: ")]
                (if (= b1 max-pins)
                  [:strike :skip]
                  (let [b2 (read-roll "Ball 2: ")]
                    (if (= (+ b1 b2) max-pins)
                      [b1 :spare]
                      [b1 b2]))))]
    (if (valid-played-frame? frame)
      frame
      (do (println "Invalid rolls, retrying.")
          (recur round)))))

(defn play-bonus [pending]
  (loop [round 1 scores []]
    (if (> round pending)
        (if (valid-bonus? scores pending)
          scores
          (do (println "Invalid bonus rolls, retrying.")
            (recur 1 [])))
        (do
          (let [b (read-roll (format "Bonus ball %d: " round))
                b-pretty (cond (= max-pins b) :strike
                               (and (peek scores) (= (+ (peek scores) b) max-pins)) :spare
                               :else b)]
            (recur (inc round) (conj scores b-pretty)))))))

(defn -main
  "Runs the bowling score input loop."
  [& args]
  (loop [index 0 scorecard blank-scorecard]
    (println (pretty-card scorecard))
    (if (and (= index max-turns) (finished-card? scorecard))
      (println "Final score: " (calculate-score scorecard))
      (let [round (inc index)
            frame (play-round round)
            pending (valid-played-frame? frame)]
            (if (= round max-turns)
              (let [bonus (play-bonus pending)]
                (recur (inc index) (assoc scorecard index (into [] (concat frame bonus)))))
                (recur (inc index) (assoc scorecard index frame)))
       ))))

