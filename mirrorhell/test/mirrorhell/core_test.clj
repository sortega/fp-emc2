(ns mirrorhell.core-test
  (:use clojure.test
        midje.sweet
        mirrorhell.core))

(fact
  (gcd 3 0) => 3
  (gcd 0 3) => 3
  (gcd 7 5) => 1
  (gcd 5 7) => 1
  (gcd 10 6) => 2
  (gcd 118 -16) => 2
  (gcd -45 -63) => 9)

(fact
  (directions 1) => #{[0 1] [1 1] [1 0] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]}
  (directions 2) => #(contains? % [2 1]))

(fact "Distance"
  (dist2 [0 0] [1 1]) => 2
  (dist2 [2 0] [4 0]) => 4
  (dist2 [0 -6] [0 10]) => 256
  (dist [0 0] [3 4]) => 5.0)

(fact "Points in rays"
  (point-in-ray? [ 3  6] (make-ray [0 0] [1 2])) => truthy
  (point-in-ray? [ 3  6] (make-ray [0 0] [2 1])) => falsey
  (point-in-ray? [-3 -6] (make-ray [0 0] [1 2])) => falsey)

(fact "Raytrace the center"
      (raytrace (make-ray 1/2 1/2 1 1) sq-room) => {:reflex [1 1]})

(fact "Raytrace a wall"
      (raytrace (make-ray [1 1] [2 1]) sq-room) => {:cut [2 1]
                                                    :segments [[[2 2] [2 0]]]})
(fact "Raytrace a corner"
      (raytrace (make-ray [1 1] [2 2]) sq-room) => {:cut [2 2]
                                                    :segments [[[0 2] [2 2]]
                                                               [[2 2] [2 0]]]}
      (raytrace (make-ray [2 1] [3 2]) column-room) => {:cut [4 3]
                                                        :segments [[[ 4 3] [ 4 4]]
                                                                   [[ 5 3] [ 4 3]]]})

(fact "Concavity test"
      (concave-from? [[[2 2] [2 4]] [[2 2] [4 2]]] [0 0]) => falsey
      (concave-from? [[[0 0] [0 6]] [[0 6] [10 6]]] [2 2]) => truthy)

(fact "Reflect in a wall and come back"
      (reflect 4 (make-ray [1 1] [2 1]) sq-room) => truthy)
(fact "Reflect in a corner and come back"
      (reflect 4 (make-ray [1 1] [2 2]) sq-room) => truthy)
(fact "Reflect in a corner and die"
      (reflect 1000 (make-ray [2 1] [3 2]) column-room) => falsey)
