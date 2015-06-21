(ns mult-tables.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def window-size [620 620])
(def circle-diameter {:x 580 :y 580})
(def multiplicator 3)

(defn half [x] (/ x 2))

(defn setup []
  ;; Set frame rate to 30 frames per second.
  (q/frame-rate 10)
  ;; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ;; setup function returns initial state. It contains
  ;; circle color and position.
  {:color 0
   :angle 0
   :modulo 10})

(defn update-state [state]
  ;; Update sketch state by changing circle color and position.
  state
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)
   :modulo (inc (:modulo state))})

(defn calc-position [modulo value]
  (let [mod-value (mod value modulo)
        segment (/ (* 2 3.141) modulo)
        angle (+ (* mod-value segment) 3.141 (/ 3.141 2))
        x (* (half (:x circle-diameter)) (q/cos angle))
        y (* (half (:y circle-diameter)) (q/sin angle))]
    [x y]))

(defn draw-modulo-pos [calc-pos value]
  (let [[x y] (calc-pos value)]
    (q/ellipse x y 5 5)))

(defn draw-state [state]
  (let [r (:modulo state)
        calc-pos (partial calc-position r)]
    ;; Clear the sketch by filling it with light-grey color.
    (q/background 240)
    ;;
    (q/fill 255)
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
      (q/ellipse 0 0 (:x circle-diameter) (:y circle-diameter))
      (doseq [i (range r)]
        (draw-modulo-pos calc-pos i))
      (doseq [i (range r)]
        (q/line (calc-pos i) (calc-pos (* i multiplicator)))))))

(q/defsketch mult-tables
  :title "Multiplication table magic"
  :size window-size
  ;; setup function called only once, during sketch initialization.
  :setup setup
  ;; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ;; This sketch uses functional-mode middleware.
  ;; Check quil wiki for more info about middlewares and particularly
  ;; fun-mode.
  :middleware [m/fun-mode])
