(ns math-practice.core
  (:require [reagent.core :as r]))
            ;[cljsjs.mathjax]))

(enable-console-print!)

;some utility functions
;
;
(defn abs [x]
  (if (< x 0)
    (* -1 x)
    x))

(defn same-sign? [x y]
  (if (or 
       (and (< 0 x ) (< 0 y)) 
       (and (> 0 x) (> 0 y)))
    true
    false))

(defn common-divisor? 
  "Determine whether div is a common divisor of a and b"
  [a b div]
  (if (and (= 0 (mod a div)) (= 0 (mod b div)))
    true
    false))



(defn reduce-fraction 
  "Fully reduce the fraction numerator/denominator"
  [numerator denominator]
  (loop [num (abs numerator)
         den (abs denominator)
         div 2
         neg (if (same-sign? numerator denominator) false true)]
    (cond
      (= 0 num) 0
      (or (> div num) (> div den )) {:numerator (if neg (* num -1) num) :denominator den}
      (common-divisor? num den div) (recur (/ num div) (/ den div) div neg)
      :else (recur num den (inc div) neg))))

(defn submit-button-handler
  "Handle clicks of the submit button by checking the answer etc"
  [state]
  (if (and (= false (:submitted @state) ) (= false (:show-solution @state)))
    (if (= (:final @state) (:answer @state))
      (swap! state update-in [:streak] inc)
      (swap! state assoc :streak 0))
    (print (str "Correct Answer: " (:final @state) " Your Answer: " (:answer @state) " State: " @state)))
  (swap! state assoc :submitted true))





;Problem generators
;
;
;
(defn division! [state]
  (let [[a b] (take 2 (repeatedly #(+ 7 (rand-int 30))))]
    (swap! state assoc :show-solution false)
    (swap! state assoc :question (str (* a b) "/" a " = "))
    (swap! state assoc :solution "sandwich")
    (swap! state assoc :final (str b))))

(defn fraction-addition! [state]
  (let [[a b c d] (take 4 (repeatedly #(inc (rand-int 9))))
        reduced (reduce-fraction (+ (* a d) (* c b)) (* b d))
        fraction (str(:numerator reduced) "/" (:denominator reduced))
        steps (str "(" a "x" d " + " b "x" c ")/(" b "x" d ")" " = " (str (+ (* a d) (* c b))) "/" (str (* b d)) " = " fraction )]
    (swap! state assoc :show-solution false)
    (swap! state assoc :question (str a "/" b " + " c "/" d " = "))
    (cond 
      (= (:numerator reduced) (:denominator reduced)) (do 
                                                        (swap! state assoc :solution (str steps " = 1"))
                                                        (swap! state assoc :final "1"))
      (= 1 (:denominator reduced)) (do 
                                     (swap! state assoc :solution (str steps " = " (:numerator reduced)))
                                     (swap! state assoc :final (:numerator reduced)))
      :else (do 
              (swap! state assoc :solution steps)
              (swap! state assoc :final fraction)))))

(defn fraction-subtraction! [state]
  (let [[a b c d] (take 4 (repeatedly #(inc (rand-int 9))))
        reduced (reduce-fraction (- (* a d) (* c b)) (* b d))
        fraction (str (:numerator reduced) "/" (:denominator reduced))
        steps (str "(" a "x" d " - " b "x" c ")/(" b "x" d ")" " = " (str (- (* a d) (* c b))) "/" (str (* b d)) " = " fraction)]
    (swap! state assoc :show-solution false)
    (swap! state assoc :question (str a "/" b " - " c "/" d " = "))
    (cond
      (= (:numerator reduced) (:denominator reduced)) (do
                                                        (swap! state assoc :solution (str steps " = 1"))
                                                        (swap! state assoc :final "1"))
      (= 1 (:denominator reduced)) (do
                                     (swap! state assoc :solution (str steps " = " (:numerator reduced)))
                                     (swap! state assoc :final (:numerator reduced)))
      :else (do
              (swap! state assoc :solution steps)
              (swap! state assoc :final fraction)))))

;(def problem-types [fraction-addition! fraction-subtraction! division!])
(def problem-types [division!])


(defn new-problem-button-handler
  "Handle clicks of the new problem button by updating the app state"
  [state]
  (let [index (rand-int (count problem-types))]
   ((get problem-types index) state)
   (swap! state assoc :submitted false)))




;Dat. Page. Content.
;
;
(defn answer-input [value]
  [:input {:type "text"
           :value (:answer @value)
           :on-change #(swap! value assoc :answer (-> % .-target .-value))}])

(defn page []
  (let [page-state (r/atom {:question "" 
                            :solution ""
                            :final ""
                            :streak 0
                            :show-solution false
                            :submitted false
                            :answer ""
                            })]
    (new-problem-button-handler page-state)
    (fn []
      [:div
       [:p (str "Correct streak: " (:streak @page-state))]
       [:p (:question @page-state)
        [answer-input page-state]
        [:input {:type "button" :value "Submit"
                 :on-click #(submit-button-handler page-state)}]]
       (if (:submitted @page-state)
         (if (= (:final @page-state) (:answer @page-state))
           [:p "Correct!"]
           [:p "Not quite, try another!"])
         [:p])
       ;(if
       ;  (:show-solution @page-state)
       ;  [:p (:solution @page-state)]
       ;  [:p])
       [:input {:type "button" :value "New Problem"
                :on-click #(new-problem-button-handler page-state)}]
       ;[:input {:type "button" :value "Show Solution"
       ;         :on-click #(swap! page-state assoc :show-solution true)}]
       ])))


(r/render-component [page]
  (. js/document (getElementById "app")))
