(ns sudoku.puzzles
  (:require [clj-http.client :as client]
            [clojure.string :as str]))

(def difficulties #{"easy" "medium" "hard" "diabolical"})

(defn get-puzzles
  ([difficulty]
   (let [endpoint (format "https://raw.githubusercontent.com/grantm/sudoku-exchange-puzzle-bank/master/%s.txt" difficulty)
         {:keys [body] :as _response} (client/get endpoint)]
     (->> body
          str/split-lines
          (map #(str/split % #" "))
          (map second))))
  ([] (get-puzzles "easy")))
