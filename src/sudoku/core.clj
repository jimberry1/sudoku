(ns sudoku.core
  (:require [clojure.set :as set]
            [sudoku.puzzles :as puzzles]
            [sudoku.puzzle-parsing :as parse])
  (:gen-class))

(defn create-or-update-set [opts value]
  (if (nil? opts)
    #{value}
    (conj opts value)))

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

(defn- coord->grid-vals
  "Finds all cell values for cells in the same grid as the provided coordinates."
  [puzzle row-idx col-idx]
  (let [grid-coordinates (coord->grid-coords row-idx col-idx)]
    (reduce (fn [acc [row col]]
              (conj acc (get-in puzzle [row col])))
            [] grid-coordinates)))

(defn- puzzle->columns [grid]
  (for [col-idx (range (count grid))]
    (mapv #(nth % col-idx) grid)))

(defn- ->grid-coords
  "Returns the top left coordinate for each grid in a 3x3 sudoku."
  []
  (for [col (range 0 9 3)
        row (range 0 9 3)]
    [col row]))

(defn- puzzle->grids [puzzle]
  (map (fn [[col row]] (coord->grid-vals puzzle row col)) (->grid-coords)))

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
  [puzzle row-idx col-idx row-vals col-vals]
  (let [grid-vals (-> puzzle (coord->grid-vals row-idx col-idx) ->missing-vals)]
    (set/intersection row-vals col-vals grid-vals)))

(defn- puzzle->possible-vals
  "Improve this by finding the missing values once per row, grid and column, then 
   passing those in"
  ([puzzle explored-options]
   (let [row-missing-vals (map ->missing-vals puzzle)
         col-missing-vals (->> puzzle puzzle->columns (map ->missing-vals))]
     (remove nil?
             (for [col (range (count puzzle))
                   row (range (count puzzle))]
               (when (zero? (get-in puzzle [row col]))
                 {:col col :row row :possible-values (set/difference
                                                      (find-possible-values puzzle row col (nth row-missing-vals row) (nth col-missing-vals col))
                                                      (get explored-options [col row]))})))))
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
          (not-empty (filter #(= 0 (count (:possible-values %))) possible-solutions))
          (do
            (when print-progress? (println "puzzle reached an unsolvable solution... backtracking to last valid state."))
            (recur (first history) (vec (rest history))))

          ;; some cells have one possible solution
          (not-empty (filter #(= 1 (count (:possible-values %))) possible-solutions))
          (do
            (when print-progress? (println "found cells with one possible solution"))
            (let [one-pos-solution (filter #(= 1 (count (:possible-values %))) possible-solutions)
                  updated-puzzle (reduce (fn [updated-puzzle {:keys [row col possible-values]}]
                                           (add-entry updated-puzzle row col (first possible-values)))
                                         puzzle one-pos-solution)]
              (if (is-puzzle-valid? updated-puzzle)
                (recur {:puzzle updated-puzzle :explored-options {}} history)
                (recur (first history) (vec (rest history))))))

          ;; Only cells with multiple options remain
          :else (do
                  (when print-progress? (println "\n\n\n no cells found with one possible solution. Branching path..."))
                  (let [{:keys [col row possible-values]} (->> possible-solutions (sort-by #(count (:possible-values %))) first)
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
                    (group-by :complete?))))
