(ns table-examlple 
  (:require [hiccup2.core :as h]
            [hiccup.page :as p]
            [hiccup.element :as e]
            [htmlkit.html :as hk]))




(defn example-1 []
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
   (hk/let-events [red green orange reset]
                  (hk/puppet [:p {:onMouseEnter (hk/fire orange)
                                  :onMouseLeave (hk/fire reset)} "I am colored"]
                             ['node.style.background "grey" [["red"    [red] :keep]
                                                             ["green"  [green] :keep]
                                                             ["orange" [orange]]
                                                             [:kept   [reset]]]]
                             ['node.style.color "grey" [["green"    [red] :keep]
                                                        ["blue"  [green] :keep]
                                                        ["white" [orange]]
                                                        [:kept   [reset]]]]) 
                  [:button {:onClick (hk/fire red)} "red"]
                  [:button {:onClick (hk/fire green)} "green"])])

(comment
  (->> (example-1)
       p/html5 str (spit "/tmp/test.html"))
  (->> (example-2)
       p/html5 str (spit "/tmp/test.html")))
