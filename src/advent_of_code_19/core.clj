(ns advent-of-code-19.core)


(defn read-number-input [filename]
  (map #(Integer. %) (clojure.string/split (slurp filename) #"\n")))

(def input1 (map #(Integer. %) (clojure.string/split (slurp "resources/input1.txt") #"\n")))
(def input1-test [1969]) ; result 654

;;; Day 1

(defn fuel-for-mass [mass]
  (int (- (Math/floor (/ mass 3)) 2)))

(defn fuel-for-module [mass]
  (let [fuel (fuel-for-mass mass)]
    (if (> fuel 0)
      (+ fuel (fuel-for-module fuel))
      0)))

(defn day1-1 [input]
  (->> input
       (map fuel-for-mass)
       (reduce +)))

(defn day1-2 [input]
  (->> input
       (map fuel-for-module)
       (reduce +)))


;;; Day 2
(defn input2 []
  (->> (clojure.string/split 
        (first (clojure.string/split (slurp "resources/input2.txt") #"\n")) #",")
       (map #(Integer. %))
       (reduce conj []))
  )

(defn calculate-window [[opcode pos1 pos2 pos-res] input]
  (let [opcodes {1 + 2 *}
        val1 (nth input pos1)
        val2 (nth input pos2)
        op (get opcodes opcode)]
    (assoc input pos-res (apply op [val1 val2]))))

(defn calculator [input index]
  (let [opcode (get input index)
        pos1 (get input (+ index 1))
        pos2 (get input (+ index 2))
        pos-res (get input (+ index 3))]
    (if (= 99 opcode)
      (reduced input)
      (calculate-window [opcode pos1 pos2 pos-res] input))))

(defn calculate [input]
  (reduce calculator input (range 0 (count input) 4)))

(defn day2-1 []
  (let [input (assoc (input2) 
                     1 12
                     2 2)]
    (calculate input)))

(defn day2-2 []
  (for [noun (range 100)
        verb (range 100)
        :let [input (assoc (input2) 1 noun 2 verb)
              result (calculate input)]
        :when (= 19690720 (first result))]
    (+ (* 100 noun) verb)))

;;; Day 3
(defn input3 []
  (clojure.string/split (slurp "resources/input3.txt") #"\n"))

(defn parse-direction [direction]
  [(subs direction 0 1) (Integer. (subs direction 1))])

(defn next-point [[x y :as point] [course distance]]
  (cond
    (= "R" course) [(+ x distance) y]
    (= "L" course) [(- x distance) y]
    (= "U" course) [x (+ y distance)]
    (= "D" course) [x (- y distance)]))

(defn next-points [start [course distance]]
  (map #(next-point start [course %]) (map inc (range distance))))

(defn route [directions]
  (reduce (fn [route direction]
            (into route (next-points (peek route) direction))) 
          [[0 0]] 
          (map parse-direction directions)))

(defn route-index [route]
  (reduce (fn [m [x y]] 
            (update-in m [x] into [y])) 
          {} route))

(defn find-intersects [route route-map]
  (reduce 
   (fn [intersects [x y :as point]] 
     (if (some #(= % y) (get route-map x))
       (conj intersects point)
       intersects)) 
   [] route))

(defn distance-to-port [[x1 y1]]
  (+ (Math/abs (- x1 0)) (Math/abs (- y1 0))))

(defn day3-1 []
  (let [input (input3)
        route1 (route (clojure.string/split (first input) #","))
        route2 (route (clojure.string/split (last input) #","))]
    (->> route1 
        (route-index)
        (find-intersects route2)
        (map distance-to-port)
        sort
        (drop 1)
        first)))

(defn distance [route point]
  (count (take-while #(not= % point) route)))

(defn day3-2 []
  (let [input (input3)
        route1 (route (clojure.string/split (first input) #","))
        route2 (route (clojure.string/split (last input) #","))
        intersects (->> route1
                        (route-index)
                        (find-intersects route2))]
    (->> intersects
         (map #(vector (distance route1 %) (distance route2 %)))
         (map #(apply + %))
         sort
         (drop 1)
         first)))


;;; Day 4
(defn digits-not-decreasing? [input]
  (->> input
       str
       (map identity)
       (map #(Character/getNumericValue %))
       (partition 2 1)
       (every? #(<= (first %) (last %)))
       ))

(defn has-same-adjacent-digits? [input]
  (->> input
       str
       (map identity)
       (map #(Character/getNumericValue %))
       (partition 2 1)
       (some #(= (first %) (last %)))
       ))

(defn matching-digits-not-in-group? [input]
  (->> input
       str
       (frequencies)
       vals
       (some #(= 2 %))
       ))

(defn password? [input]
  (let [pred (every-pred number?
                         #(>= % 100000)
                         #(< % 1000000))]
    (pred input)))

(defn handle-equal [{last :last count :count matches :matches position :position :as state} number]
  (if (and (= position 6) (= count 1))
    (assoc state :matches true )
    {:last number
     :count (inc count)
     :matches matches
     :position (inc position)}))

(defn handle-greater [{last :last count :count matches :matches position :position :as state} number]
  (if (= count 2)
    (assoc state :matches true :count (inc count) position (inc position) :last number)
    {:last number
     :count 1
     :matches matches
     :position (inc position)}))

(defn two-matching-adjacent-digits? [input]
  (->> input
       str
       (map identity)
       (map #(Character/getNumericValue %))
       (reduce 
        (fn [{last :last count :count matches :matches position :position :as state} number]
          (cond
            (nil? last) {:last number :count 1 :position 2}
            (< number last) (reduced {:matches false})
            (= last number) (handle-equal state number)
            (> number last) (handle-greater state number))) {:matches false :position 1})
       (:matches)))

(defn day4-1 []
  (->> (range 153517 630395)
       (filter password?)
       (filter two-matching-adjacent-digits?)
       (count)))



(defn day4-2 []
  (->> (range 153517 630395)
       (filter password?)
       (filter two-matching-adjacent-digits?)
       (count)))


