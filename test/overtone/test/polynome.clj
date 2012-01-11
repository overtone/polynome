(ns overtone.test.polynome
  (:use [overtone.polynome] :reload)
  (:use [midje.sweet]
        [overtone.grid.dummy])
  (:import java.io.StringWriter))

(fact "range-x and range-y return the dimensions of the grid"
  (let [polynome (init (make-dummy 32 16))]
    [(range-x polynome) (range-y polynome)] => [32 16]))

(facts "The max coordinate is one less than the range"
  (let [polynome (init (make-dummy 15 12))]
    (fact (max-x polynome) => 14)
    (fact (max-y polynome) => 11)))

(facts "The number of coords should be x*y"
  (let [polynome1 (init (make-dummy 10 10))
        polynome2 (init (make-dummy 11 11))]
    (fact (count (coords polynome1)) => 100)
    (fact (count (coords polynome2)) => 121)))

(fact "led-on should update the led-activation state"
  (let [polynome (init (make-dummy 2 2))]
    (fact
      (do (led-on polynome 0 0)
          (if (await-for 1000 (state-agent polynome))
            (led-activation polynome)
            :timed-out))
      => {[0 0] 1 [0 1] 0 [1 0] 0 [1 1] 0}
      )))

(fact "led-off should update the led-activation state"
  (let [polynome (init (make-dummy 2 2))]
    (fact
      (do (led-on polynome 0 0)
          (led-off polynome 0 0)
          (if (await-for 1000 (state-agent polynome))
            (led-activation polynome)
            :timed-out))
      => {[0 0] 0 [0 1] 0 [1 0] 0 [1 1] 0}
      )))