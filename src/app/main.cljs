(ns app.main (:require [reagent.core :as r]))

(defonce state (r/atom {:world []}))

(defn get-num [] (let [x (Math/round (rand 10))] (if (= x 5) 1 0)))

(defn get-random [] (Math/round (rand)))

(defn create-world! [size] (swap! state assoc :world (map #(vec (for [x (range
  size)] (get-random))) (range size))))

(defn update-world! [next-world] (print "update!") (swap! state assoc :world
  next-world))

(defn val-at [ridx cidx m] (nth (nth m ridx) cidx))

(defn is-alive? [cell-val] (= cell-val 1))

(defn first-row? [ridx] (= ridx 0))

(defn last-row? [ridx m] (= ridx (- (count m) 1)))

(defn first-col? [cidx] (= cidx 0))

(defn last-col? [cidx m] (= cidx (- (count m) 1)))

(defn get-neighbors [ridx cidx m] (let [up (if (first-row? ridx) 0 (val-at (-
  ridx 1) cidx m)) dwn (if (last-row? ridx m) 0 (val-at (+ ridx 1) cidx m)) lft
  (if (first-col? cidx) 0 (val-at ridx (- cidx 1) m)) rt (if (last-col? cidx m)
  0 (val-at ridx (+ cidx 1) m)) lup (if (or (first-row? ridx) (first-col? cidx))
  0 (val-at (- ridx 1) (- cidx 1) m)) ldwn (if (or (last-row? ridx m)
  (first-col? cidx)) 0 (val-at (+ ridx 1) (- cidx 1) m)) rup (if (or (first-row?
  ridx) (last-col? cidx m)) 0 (val-at (- ridx 1) (+ cidx 1) m)) rdwn (if (or
  (last-row? ridx m) (last-col? cidx m)) 0 (val-at (+ ridx 1) (+ ridx 1) m))]
  [up rt dwn lft lup ldwn rup rdwn]))

(defn get-next-state [curr-val nbrs] (let [alive? (= curr-val 1) live-nbrs
  (reduce + nbrs)] (cond (and alive? (< live-nbrs 2)) 0 (and alive? (or (= 2
  live-nbrs) (= 3 live-nbrs))) 1 (and alive? (> live-nbrs 3)) 0 (and (not
  alive?) (= live-nbrs 3)) 1 :else 0)))

(defn get-next-world [old-world] (keep-indexed (fn [ridx row] (map-indexed (fn
  [cidx value] (->> (get-neighbors ridx cidx old-world)
               (get-next-state (val-at ridx cidx old-world)))) row))
    old-world))

(defn title []
  [:h1 "Conway's Game of Life"])

(def alive \u2b1b)
(def dead \u2b1c)

(defn cell [alive?]
  [:span.cell (if alive? alive dead)])

(defn row [row]
  [:div {:className "row"}
   (map-indexed (fn [idx val] ^{:key idx} [cell (is-alive? val)]) row)
   [:br]])

(defn game-matrix [m]
  (js/console.log (clj->js m))
  (def content (map-indexed (fn [idx r] ^{:key idx} [row r]) m))
  [:div content])

(defn app [data]
  (let [world (:world @state)]
    [:div.app
      [title]
      [game-matrix world]]))

(defn mount []
  (r/render [app] (js/document.getElementById "app")))

(defn init []
  (print "Initalizing game world!")
  (create-world! 30))

(defn main! []
  (init)
  (mount)
  ; (js/console.log "about to get next state")
  ; (js/console.log (clj->js (get-next-world (:world @state))))
  ; (update-world! (get-next-world (:world @state)))
  (js/setInterval #(update-world! (get-next-world (:world @state))) 1000)
  (println "App loaded!"))
