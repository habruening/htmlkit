(ns table-examlple 
  (:require [hiccup2.core :as h]
            [hiccup.page :as p]
            [hiccup.element :as e]
            [htmlkit.html :as hk]))




(defn create []
  [:body
    (hk/load-event-handling)
    (hk/let-events [cancel]
                (hk/let-event-map [events [:onClick :onMouseEnter :onMouseLeave]]
                               (hk/add-events [:p "I send events"] events)
                               (hk/puppet [:p "I react"]
                                       ['node.style.display "auto" [["none" [(events :onClick)]]
                                                                    ["block" [cancel]]]]
                                       ['node.style.visibility "visible" [["hidden" [(events :onMouseEnter)]]
                                                                          ["visible" [(events :onMouseLeave)]]]]))
                   [:button {:onClick (hk/fire cancel)} "cancel"])]
  
  
  
  )

(comment
  (->> (create)
       p/html5 str (spit "/tmp/test.html")))
