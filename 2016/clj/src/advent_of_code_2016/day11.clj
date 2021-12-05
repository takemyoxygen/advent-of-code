(ns advent-of-code-2016.day11)

(defn floor-to-index
  [floor]
  (case
    floor
    "first" 1
    "second" 2
    "third" 3
    "fourth" 4))

(defn parse-item
  [item]
  (let [[_ material _ type] (re-matches #"a (.+?)(-compatible)? (generator|microchip)" item)]
    {:material material :type type}))

(defn parse-content
  [content]
  (if (= content "nothing relevant")
    #{}
    (let [items (clojure.string/split content #", (and )?| and ")]
      (set (map parse-item items)))))

(defn parse-line
  [line]
  (let [[_ floor content-string] (re-matches #"The (.*) floor contains (.*)." line)
        index (floor-to-index floor)
        content (parse-content content-string)]
    {index content}))

(defn parse-input
  [raw-input]
  (apply merge (map parse-line (clojure.string/split-lines raw-input))))

(defn initial-state
  [parsed-input]
  (merge {:elevator 1} parsed-input))

(defn floor-reachable-from
  [floor]
  (->> [(inc floor) (dec floor)]
       (filter (fn [fl] (and (> fl 0) (< fl 5))))))

(defn objects-to-move
  [all-objects]
  (loop [acc [] [current & rest] all-objects]
    (if (nil? current)
      acc
      (let [movements-incl-current (cons [current] (map (fn [x] [current x]) rest))
            updated-acc (concat acc movements-incl-current)]
        (recur updated-acc rest)))))

(defn next-state
  [initial-state to-move move-floor]
  (let [curr-floor (:elevator initial-state)]
    (-> initial-state
        (assoc :elevator move-floor)
        (update curr-floor (fn [curr] (apply disj curr to-move)))
        (update move-floor (fn [next] (apply conj next to-move))))))

(defn cartesian
  [xs ys]
  (mapcat (fn [x] (map (fn [y] [x y]) ys)) xs))

(defn is-valid-floor?
  [floor-objects]
  (let [{microchips "microchip" generators "generator"} (group-by :type floor-objects)
        is-valid-microchip? (fn [{material :material}] (some (fn [gen] (= (:material gen) material)) generators))]
    (or (empty? generators)
        (every? is-valid-microchip? microchips))))

(defn is-valid-state?
  [state source-floor]
  (and (is-valid-floor? (get state source-floor)) (is-valid-floor? (get state (:elevator state)))))

(defn next-states
  [state]
  (let [current-floor (:elevator state)
        reachable-floors (floor-reachable-from current-floor)
        potential-moves (objects-to-move (get state current-floor))
        moves-and-floors (cartesian reachable-floors potential-moves)
        new-states (map (fn [[floor moves]] (next-state state moves floor)) moves-and-floors)]
    (filter (fn [new-state] (is-valid-state? new-state current-floor)) new-states)))

(defn final-state
  [initial-state]
  (let [floors (filter number? (keys initial-state))
        max-floor (apply max floors)
        all-objects (reduce (fn [acc [k v]] (if (number? k) (apply conj acc v) acc)) #{} initial-state)]
    (reduce (fn [acc fl] (assoc acc fl (if (= fl max-floor) all-objects #{}))) {:elevator max-floor} floors)))

(defn solve
  [initial-state]
  (let [final-st (final-state initial-state)]
    (loop [current-states [initial-state] known-states #{} steps 1]
      (let [next-st (set (mapcat next-states current-states))
            new-next-st (clojure.set/difference next-st known-states)]
        (if (contains? new-next-st final-st)
          steps
          (recur new-next-st (apply conj known-states new-next-st) (inc steps)))))))


(defn solve-part1
  [input]
  (-> input
      (parse-input)
      (initial-state)
      (solve)))

(defn solve-part2
  [input]
  (let [adjustment [{:material "elerium" :type "generator"}
                    {:material "elerium" :type "microchip"}
                    {:material "dilithium" :type "generator"}
                    {:material "dilithium" :type "microchip"}]]
    (-> input
        (parse-input)
        (initial-state)
        (update 1 (fn [objects] (apply conj objects adjustment)))
        (solve))))
