(ns sudoku.core
  (:require [clojure.set :as set]
            [sudoku.puzzles :as puzzles]
            [sudoku.puzzle-parsing :as parse])
  (:gen-class))

(defn create-or-update-set [val new-value]
  (if (nil? val)
    #{new-value}
    (conj val new-value)))

(def ^:private allowed-values #{1 2 3 4 5 6 7 8 9})

(defn- ->missing-vals [vals]
  (set/difference allowed-values vals))

(defn- is-subsection-complete? [subsection-vals]
  (let [subsection-set (set subsection-vals)]
    (= allowed-values subsection-set)))

(defn- coord->grid-coords
  "Given row and column indices, finds all indices of cells in the same grid."
  [row col]
  (let [box-start-row (* 3 (quot row 3))
        box-start-col (* 3 (quot col 3))]
    (for [r (range box-start-row (+ box-start-row 3))
          c (range box-start-col (+ box-start-col 3))]
      [r c])))

(def memo-coord->grid-coords (memoize coord->grid-coords))

(defn- coord->grid-vals
  "Finds all cell values for cells in the same grid as the provided coordinates."
  [puzzle row-idx col-idx]
  (let [grid-coordinates (memo-coord->grid-coords row-idx col-idx)]
    (reduce (fn [acc [row col]]
              (conj acc (get-in puzzle [row col])))
            [] grid-coordinates)))

(defn- puzzle->columns [puzzle]
  (for [col-idx (range (count puzzle))]
    (mapv #(nth % col-idx) puzzle)))

(def cells
  (for [col (range 9)
        row (range 9)]
    [col row]))

(def grid-coords
  "Returns the top left coordinate for each grid in a 3x3 sudoku."
  (for [row (range 0 9 3)
        col (range 0 9 3)]
    [col row]))

(defn coord->grid-index
  "Given the column and row coordinates of a cell, returns the
   index (0-8) of the grid it is in"
  [col row]
  (let [col-idx (quot col 3)
        row-idx (quot row 3)]
    (+ (* 3 row-idx) col-idx)))

(defn- puzzle->grids [puzzle]
  (map (fn [[col row]] (coord->grid-vals puzzle row col)) grid-coords))

(defn- occurs-at-most-once?
  ([values]
   (let [val-frequencies (frequencies values)]
     (or (empty? val-frequencies)
         (->> val-frequencies vals (every? #(= 1 %))))))
  ([values allowed-values]
   (->> values (filter #(allowed-values %)) occurs-at-most-once?)))

(defn- is-puzzle-complete? [puzzle]
  (let [rows-complete? (every? is-subsection-complete? puzzle)
        cols-complete? (->> puzzle puzzle->columns (every? is-subsection-complete?))
        grids-complete? (->> puzzle puzzle->grids (every? is-subsection-complete?))]
    (and rows-complete? cols-complete? grids-complete?)))

(defn- is-puzzle-valid? [puzzle & {:keys [debug?]}]
  (let [rows-complete? (every? #(occurs-at-most-once? % allowed-values) puzzle)
        cols-complete? (->> puzzle puzzle->columns (every? #(occurs-at-most-once? % allowed-values)))
        grids-complete? (->> puzzle puzzle->grids (every? #(occurs-at-most-once? % allowed-values)))]
    (if debug?
      {:valid? (and rows-complete? cols-complete? grids-complete?)
       :rows rows-complete? :cols cols-complete? :grid grids-complete?}
      (and rows-complete? cols-complete? grids-complete?))))

(defn- add-entry [puzzle row-idx column-idx value]
  (assoc-in puzzle [row-idx column-idx] value))

(defn- find-possible-values
  "Finds all possible entries for a puzzle at the given row and column index."
  [row-vals col-vals grid-vals seen-vals]
  (let [allowed-vals (set/intersection row-vals col-vals grid-vals)]
    (set/difference allowed-vals seen-vals)))

(defn- puzzle->possible-vals
  "Improve this by finding the missing values once per row, grid and column, then 
   passing those in"
  ([puzzle explored-options]
   (let [row-missing-vals (map ->missing-vals puzzle)
         col-missing-vals (->> puzzle puzzle->columns (map ->missing-vals))
         grid-missing-vals (->> puzzle puzzle->grids (map ->missing-vals))]
     (reduce (fn [possible-vals-map [col row]]
               (if (zero? (get-in puzzle [row col]))
                 (let [row-vals (nth row-missing-vals row)
                       col-vals (nth col-missing-vals col)
                       grid-vals (nth grid-missing-vals (coord->grid-index col row))
                       seen-vals (get explored-options [col row])
                       possible-vals (find-possible-values row-vals col-vals grid-vals seen-vals)
                       new-entry {:col col :row row :possible-values possible-vals}]
                   (update possible-vals-map (count possible-vals) conj new-entry))
                 possible-vals-map))
             {} cells)))
  ([puzzle] (puzzle->possible-vals puzzle {})))

(defn solve-puzzle-string
  "Entry point to solving puzzles. Needs to be provided with a 3x3 sudoku string, where blanks are 0."
  [puzzle-str & {:keys [print-progress?]}]
  (let [starting-puzzle (parse/string->puzzle puzzle-str)]
    (loop [{:keys [puzzle explored-options]} {:puzzle starting-puzzle :explored-options {}}
           history []]
      (let [possible-solutions (puzzle->possible-vals puzzle explored-options)]
        (cond
          (nil? puzzle)
          (do (when print-progress? (println "\n\n\n puzzle is unsolveable"))
              {:complete? false :puzzle puzzle :starting-puzzle starting-puzzle})

          (is-puzzle-complete? puzzle)
          (do (when print-progress? (println "\n\n\n puzzle is complete"))
              {:complete? true :puzzle puzzle})

          ;; Some cells have no possible solutions
          (contains? possible-solutions 0)
          (do
            (when print-progress? (println "puzzle reached an unsolvable solution... backtracking to last valid state."))
            (recur (first history) (vec (rest history))))

          ;; some cells have one possible solution
          (contains? possible-solutions 1)
          (do
            (when print-progress? (println "found cells with one possible solution"))
            (let [one-pos-solution (get possible-solutions 1)
                  updated-puzzle (reduce (fn [updated-puzzle {:keys [row col possible-values]}]
                                           (add-entry updated-puzzle row col (first possible-values)))
                                         puzzle one-pos-solution)]
              (if (is-puzzle-valid? updated-puzzle)
                (recur {:puzzle updated-puzzle :explored-options {}} history)
                (recur (first history) (vec (rest history))))))

          ;; Only cells with multiple options remain
          :else (do
                  (when print-progress? (println "\n\n\n no cells found with one possible solution. Branching path..."))
                  (let [{:keys [col row possible-values]} (->> (range 2 10) (some #(get possible-solutions %)) first)
                        chosen-value (first possible-values)
                        updated-puzzle (add-entry puzzle row col chosen-value)
                        updated-explored-options (update explored-options [col row] create-or-update-set chosen-value)]
                    (recur {:puzzle updated-puzzle :explored-options {}}
                           (conj history {:puzzle puzzle :explored-options updated-explored-options})))))))))

(comment
  (def puzzles (puzzles/get-puzzles "hard"))

  (def results (->> puzzles
                    (take 1000)
                    (map solve-puzzle-string)
                    (group-by :complete?)))

  (time (solve-puzzle-string "050908600800006007006020000009000070203000809010000400000030700900800004005604030" :print-progress? false)))
