(ns mult-tables.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; The idea for this visualization comes from this video
;; https://www.youtube.com/watch?v=-X49VQgi86E&feature=youtu.be

(def window-size [620 630])

(def circle-diameter {:x 580 :y 580})

(def offset 20)

(def initial-state {:modulo 16
                    :multiplicator 7
                    :next-modulo-fn identity})

(defn change-in-state [key f state]
  (assoc state key (f (key state))))

(defn key-typed [state key]
  (condp = (:key key)
    :+ (change-in-state :multiplicator inc state)
    :- (change-in-state :multiplicator dec state)
    :h (assoc state :next-modulo-fn identity)
    :a (assoc state :next-modulo-fn inc)
    :n (change-in-state :modulo inc state)
    :p (change-in-state :modulo dec state)
    :r initial-state
    state))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  ;; The initial state
  initial-state)

(defn update-state [state]
  (assoc state :modulo ((:next-modulo-fn state) (:modulo state))))

(defn calc-position [modulo value]
  (let [mod-value (mod value modulo)
        segment (/ q/TWO-PI modulo)
        circle-x-radius (/ (:x circle-diameter) 2)
        circle-y-radius (/ (:y circle-diameter) 2)
        angle (+ (* mod-value segment) q/PI q/HALF-PI)
        x (* circle-x-radius (q/cos angle))
        y (* circle-y-radius (q/sin angle))]
    [x y]))

(defn draw-modulo-pos [modulo value]
  (let [[x y] (calc-position modulo value)]
    (q/ellipse x y 5 5)))

(defn draw-line [modulo v1 v2]
  (q/line (calc-position modulo v1) (calc-position modulo v2)))

(defn draw-text [modulo multiplicator]
  (q/fill 0)
  (q/text (str "Mod:  " modulo) 20 600)
  (q/text (str "Mult: " multiplicator) 20 620))

(defn draw-state [state]
  ;; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  (q/fill 255)
  (let [{:keys [:modulo :multiplicator]} state]
    (q/with-translation [(+ offset (/ (:x circle-diameter) 2))
                         (+ offset (/ (:y circle-diameter) 2))]
      (q/ellipse 0 0 (:x circle-diameter) (:y circle-diameter))
      (doseq [i (range modulo)]
        (draw-modulo-pos modulo i)
        (draw-line modulo i (* i multiplicator))))
    (draw-text modulo multiplicator)))

(q/defsketch mult-tables
  :title "Multiplication magic"
  :size window-size
  ;; setup function called only once, during sketch initialization.
  :setup setup
  ;; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :key-typed key-typed
  ;; This sketch uses functional-mode middleware.
  ;; Check quil wiki for more info about middlewares and particularly
  ;; fun-mode.
  :middleware [m/fun-mode])
