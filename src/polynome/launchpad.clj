(ns polynome.launchpad
  (:require [polynome.core :as poly])
  (:use [overtone.midi]
        [polynome.polynome]))

(def RED 15)

(defn make-launchpad []
  (if-let [launchpad-in (midi-in "Launchpad")]
    (if-let [launchpad-out (midi-out "Launchpad")]
      (reify Polynome
        (on-action [this f group name] ; currently ignoring group and name
          (midi-handle-events launchpad-in
                              (fn [event ts]
                                (let [note (:note event)
                                      y    (unchecked-divide-int note 16)
                                      x    (rem note 16)]
                                  (if (zero? (:vel event))
                                    (f :release x y)
                                    (f :press   x y))))))
        (clear-all-leds [this]
          "FIXME: unimplemented")
        (illuminate-all-leds [this]
          "FIXME: unimplemented")
        (led-on [this x y]
          (let [note (+ (* y 16) x)]
            (midi-note-on launchpad-out note RED)))
        (led-off [this x y]
          (let [note (+ (* y 16) x)]
            (midi-note-off launchpad-out note)))
        (led-frame [this idx & rows]
          "FIXME: unimplemented"))
      (throw (Exception. "Found launchpad for input but couldn't find it for output")))
    (throw (Exception. "Couldn't find launchpad"))))

(defn init-launchpad []
  (poly/init (make-launchpad) :launchpad :north 8 8))