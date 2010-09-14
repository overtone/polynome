(ns polynome.core
  (:require [monome-serial.monome :as monome]))

(def monomes (atom {}))

(defn register
  "Register your monome. If just the path to the monome is supplied it is assumed to be the :default. However, it is possible to also pass a keyword name to enable you to register multiple monomes"
  ([path] (register :default path))
  ([name path]
     (let [monome (monome/connect path)]
       (swap! monomes assoc name monome))))

;;TODO Implement me!
(defn max-x
  "Returns the maximum x coord of the specified or default monome"
  ([] (max-x :default))
  ([name]
     (let [monome (name @monomes)]
       7)))

;;TODO Implement me!
(defn max-y
  "Returns the maximum y coord of the specified or default monome"
  ([] (max-y :default))
  ([name]
     (let [monome (name @monomes)]
       7)))

(defn range-x
  "Returns the number of buttons along the x axis of the specified or default monome"
  ([] (range-x :default))
  ([name]
     (let [monome (name @monomes)]
       (+ 1 (max-x)))))

(defn range-y
  "Returns the number of buttons along the y axis of the specified or default monome"
  ([] (range-y :default))
  ([name]
     (let [monome (name @monomes)]
       (+ 1 (max-y)))))

(defn rand-x
  "Returns a random x coordinate for the speicified or default monome"
  ([] (rand-x :default))
  ([name]
     (rand-nth (range (range-x name)))))

(defn rand-y
  "Returns a random y coordinate for the speicified or default monome"
  ([] (rand-y :default))
  ([name]
     (rand-nth (range (range-y name)))))

(defn coords
  "Returns a lazy sequence of all pairs of x y coords for the specified or default monome"
  ([] (coords :default))
  ([name]
     (let [monome (name @monomes)]
       (for [x (range (range-x name))
             y (range (range-y name))]
         [x y]))))

(defn map-coords
  [monome x y]
  [x y])

(defn- fetch-monome
  "Fetches the monome registered with the specific name. Raises an exception if a match isn't found"
  [name]
  (let [monome (name @monomes)]
    (if-not monome
      (throw (Exception. (str "Couldn't find the monome with name " name ))))
    monome))

(defn led-on
  "Turns on the led of either the specified or default monome"
  ([x y] (led-on :default x y))
  ([name x y]
     (let [m (fetch-monome name)]
       (monome/led-on m (map-coords m x y)))))

(defn led-off
  "Turns off the led of either the specified or default monome"
  ([x y] (led-off :default x y))
  ([name x y]
     (let [m (fetch-monome name)]
       (monome/led-off m (map-coords m x y)))))
