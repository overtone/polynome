(ns polynome.core
  (:use [clojure.contrib.ns-utils :only [immigrate]])
  (:require  monome-serial.monome
             monome-serial.monome-at))

(immigrate
 'monome-serial.monome
 'monome-serial.monome-at)

(defn init  "Initialise a monome. Raises an exception if the supplied path isn't valid or is already in use"
  [path]
  (let [monome (connect path)]
    (assoc monome ::polynome {:max-x 7
                              :max-y 7
                              :range-x 8
                              :range-y 8})))

(defn max-x
  "Returns the monome's maximum x coord"
  [m]
  (get-in m [::polynome :max-x]))

(defn max-y
  "Returns the monome's maximum y coord"
  [m]
  (get-in m [::polynome :max-y]))

(defn range-x
  "Returns the number of buttons on the x axis"
  [m]
  (get-in m [::polynome :range-x]))

(defn range-y
  "Returns the number of buttons on the y axis"
  [m]
  (get-in m [::polynome :range-y]))

(defn rand-x
  "Returns a random x coordinate"
  [m]
  (rand-nth (range (range-x m))))

(defn rand-y
  "Returns a random y coordinate"
  [m]
  (rand-nth (range (range-y m))))

(defn coords
  "Returns a lazy sequence of all pairs of x y coords"
  [m]
  (for [x (range (range-x m))
        y (range (range-y m))]
    [x y]))

(defn button-id
  "Returns a unique integer id for a given set of coordinates."
  [m x y]
  (+ (* (range-y m) y) x))

(defn map-coords
  [monome x y]
  [x y])


