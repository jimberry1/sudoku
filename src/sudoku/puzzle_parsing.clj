(ns sudoku.puzzle-parsing)

(defn generate-puzzle [n-grids]
  (vec (repeat n-grids (vec (repeat n-grids nil)))))

(defn string->puzzle [sudoku-str]
  (->> sudoku-str
       (filter #(Character/isDigit %))
       (map #(Character/getNumericValue %))
       (partition 9)
       (mapv vec)))