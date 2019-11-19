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
      (or (> div (/ num 2)) (> div (/ den 2))) {:numerator num :denominator den}
      (common-divisor? num den div) (recur (/ num div) (/ den div) div)
      :else (recur num den (inc div)))))

;Problem generators
;
;
(defn fraction-addition [some-atom]
  (let [[a b c d] (take 4 (repeatedly #(inc (rand-int 9))))
        reduced (reduce-fraction (+ (* a d) (* c b)) (* b d))]
    (swap! some-atom assoc :show-solution false)
    (swap! some-atom assoc :question (str a "/" b " + " c "/" d " = "))
    (swap! some-atom assoc :solution (str (:numerator reduced) "/" (:denominator reduced)))))
  

;Dat. Page. Content.
;
;
(defn page []
  (let [page-state (r/atom {:question "" 
                            :solution ""
                            :streak 0
                            :show-solution false
                            })]
    (fraction-addition page-state)
    (fn []
      [:div
       [:p (:question @page-state) ]
       (if (:show-solution @page-state) [:p (:solution @page-state)] [:p ])
       [:input {:type "button" :value "New Problem"
                :on-click #(fraction-addition page-state)}]
       [:input {:type "button" :value "Show Solution"
                :on-click #(swap! page-state assoc :show-solution true)}]])))


(r/render-component [page]
  (. js/document (getElementById "app")))
