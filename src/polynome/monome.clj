(ns polynome.monome
  (:require  [monome-serial.core :as monome-core]
             [monome-serial.led :as monome]
             [monome-serial.event-handlers :as handlers]
             [polynome.core :as poly])
  (:use [polynome.polynome]))

(defn- make-monome [path]
  (let [monome (monome-core/connect path)]
    (reify Polynome
      (on-action [this f group name]
        (handlers/on-action m f group name))
      (clear-all-leds [this]
        (monome/clear monome))
      (illuminate-all-leds [this]
        (monome/all monome))
      (led-on [this x y]
        (monome/led-on monome x y))
      (led-off [this x y]
        (monome/led-off monome x y))
      (led-frame [this idx & rows]
        (apply monome/frame monome idx rows))
      (is-connected? [this]
        (monome-core/connected? monome))
      (disconnect [this]
        (monome-core/disconnect monome)))))

(def MONOME-KINDS
  {
   :64n       [[8   8] :north]
   :64e       [[8   8] :east]
   :64s       [[8   8] :south]
   :64w       [[8   8] :west]
   :128ln     [[16  8] :north]
   :128ls     [[16  8] :south]
   :128pw     [[8  16] :west]
   :128pe     [[8  16] :east]
   :256n      [[16 16] :north]
   :256e      [[16 16] :east]
   :256s      [[16 16] :south]
   :256w      [[16 16] :west]
   :dummy64   [[8   8] :north]
   :dummy128l [[16  8] :north]
   :dummy128p [[8  16] :west]
   :dummy256  [[8   8] :north]
   })

(defn- detect-kind
  [path]
  (condp re-find path
    #"-m64-"      :64n
    #"-m128-"     :128ln
    #"-m256-"     :256n
    #"dummy64"    :dummy64
    #"dummy128"   :dummy128l
    #"dummy128l"  :dummy128l
    #"dummy128p"  :dummy128p
    #"dummy256"   :dummy256
    #"dummy"      :dummy64
    :unknown))


(defn- monome-info
  [kind]
  (let [info (get MONOME-KINDS kind)]
    (when-not info
      (throw (Exception. (str "Unknown monome kind " [kind] ". Expected one of " (keys MONOME-KINDS)))))

    info))

(defn init-monome
  "Initialise a monome. When passed only a path, will attempt to infer the kind
  of monome from the pathname. Where this isn't possible, you can either specify
  the kind as a keyword (64n, 128pw , 256s etc. where the number represents the
  number of buttons on the specific monome and the letters represent the cable
  position n,e,s,w and orientation for 128 monomes - p and l for portrait and
  landscape).

  It is also possible to explicitly specify the kind, cable orientation (:north
  :east :south or :west) and num cols and rows.

  If you use one of the :dummy kinds then polynome won't attempt to connect to
  a physical monome - allowing for mocking and testing when a real connection
  isn't feasible.

  Raises an exception if the supplied path isn't valid or is already in use"
  ([path] (init-monome path (detect-kind path)))
  ([path kind] (let [[[n-cols n-rows] cable] (monome-info kind)]
                 (init-monome path kind cable n-cols n-rows)))
  ([path kind cable n-cols n-rows]
     (poly/init (make-monome path) kind cable n-cols n-rows)))