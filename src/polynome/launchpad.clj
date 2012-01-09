(ns polynome.launchpad
  (:use [overtone.midi]
        [polynome.polynome]))

(defn- make-launchpad []
  (if-let [launchpad (midi-in "Launchpad")]
    (reify Polynome
      (on-action [this f group name] ; currently ignoring group and name
        (midi-handle-events launchpad
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
        "FIXME: unimplemented")
      (led-off [this x y]
        "FIXME: unimplemented")
      (led-frame [this idx & rows]
        "FIXME: unimplemented"))
    (throw (Exception. "Didn't find launchpad"))))

(defn init-launchpad []
  (poly/init (make-launchpad) :launchpad :north 8 8))