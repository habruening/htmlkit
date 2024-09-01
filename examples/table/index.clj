(ns table-examlple 
  (:require [hiccup2.core :as h]
            [hiccup.page :as p]
            [hiccup.element :as e]
            [htmlkit.html :as hk]))




(defn example-1 [] ; to be updated
  [:body
    (hk/load-event-handling)
    (hk/let-events [reset]
                (hk/let-event-map [events [:onClick :onMouseEnter :onMouseLeave]]
                               (hk/add-events [:p "Hover or click me!"] events)
                               (hk/puppet [:p "I will react."]
                                       ['node.style.display "auto" [["none" [(events :onClick)]]
                                                                    ["block" [reset]]]]
                                       ['node.style.visibility "visible" [["hidden" [(events :onMouseEnter)]]
                                                                          ["visible" [(events :onMouseLeave)]]]]))
                   [:button {:onClick (hk/fire reset)} "reset"])]
  
  
  
  )

(defn example-2 []
  [:body
   (hk/load-event-handling)
   (hk/let-events [red green hover reset select]
                  (hk/toggle-node [:p {:onMouseEnter (hk/fire hover)
                                       :onMouseLeave (hk/fire reset)
                                       :onClick (hk/fire select)} "I start white and get colored"]
                                  ['node.style.background [["red"    [red] :keep]
                                                           ["green"  [green] :keep]
                                                           ["grey" [hover]] 
                                                           [["orange" "grey"] [select] :keep]
                                                           [:kept    [reset]]
                                                           ["lightgrey" [] :init]]]) 
                  [:button {:onClick (hk/fire red)} "red"]
                  [:button {:onClick (hk/fire green)} "green"])])

(comment
  (->> (example-1)
       p/html5 str (spit "/tmp/test.html"))
  (->> (example-2)
       p/html5 str (spit "/tmp/test.html")))
