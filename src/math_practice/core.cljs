(ns math-practice.core
  (:require [reagent.core :as r]))
            ;[cljsjs.mathjax]))

(enable-console-print!)

;some utility functions
;
;
(defn common-divisor? 
  "Determine whether div is a common divisor of a and b"
  [a b div]
  (if (and (= 0 (mod a div)) (= 0 (mod b div)))
    true
    false))

(defn reduce-fraction 
  "Fully reduce the fraction numerator/denominator"
  [numerator denominator]
  (loop [num numerator
         den denominator
         div 2]
    (cond
      (or (> div num) (> div den )) {:numerator num :denominator den}
      (common-divisor? num den div) (recur (/ num div) (/ den div) div)
      :else (recur num den (inc div)))))

(defn submit-button-handler
  "Handle clicks of the submit button by checking the answer etc"
  [state]
  (if (and (= false (:submitted @state) ) (= false (:show-solution @state)))
    (if (= (:final @state) (:answer @state))
      (swap! state update-in [:streak] inc)
      (swap! state assoc :streak 0))
    nil)
  (swap! state assoc :submitted true))





;Problem generators
;
;
(defn fraction-addition [some-atom]
  (let [[a b c d] (take 4 (repeatedly #(inc (rand-int 9))))
        reduced (reduce-fraction (+ (* a d) (* c b)) (* b d))
        fraction (str(:numerator reduced) "/" (:denominator reduced))
        steps (str "(" a "x" d " + " b "x" c ")/(" b "x" d ")" " = " (str (+ (* a d) (* c b))) "/" (str (* b d)) " = " fraction )]
    (swap! some-atom assoc :show-solution false)
    (swap! some-atom assoc :question (str a "/" b " + " c "/" d " = "))
    (cond 
      (= (:numerator reduced) (:denominator reduced)) (do 
                                                        (swap! some-atom assoc :solution (str steps " = 1"))
                                                        (swap! some-atom assoc :final "1"))
      (= 1 (:denominator reduced)) (do 
                                     (swap! some-atom assoc :solution (str steps " = " (:numerator reduced)))
                                     (swap! some-atom assoc :final (:numerator reduced)))
      :else (do 
              (swap! some-atom assoc :solution steps)
              (swap! some-atom assoc :final fraction)))))
  
(defn new-problem-button-handler
  "Handle clicks oft he new problem button by updating the app state"
  [state]
  (fraction-addition state)
  (swap! state assoc :submitted false))

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
       [:p (str "Correct answers: " (:streak @page-state))]
       [:p (:question @page-state) 
        [answer-input page-state]
        [:input {:type "button" :value "Submit"
                 :on-click #(submit-button-handler page-state)}]]
       (if
         (:show-solution @page-state)
         [:p (:solution @page-state)]
         [:p])
       [:input {:type "button" :value "New Problem"
                :on-click #(new-problem-button-handler page-state)}]
       [:input {:type "button" :value "Show Solution"
                :on-click #(swap! page-state assoc :show-solution true)}]])))


(r/render-component [page]
  (. js/document (getElementById "app")))
