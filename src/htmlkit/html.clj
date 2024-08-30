(ns htmlkit.html
  (:require [htmlkit.js :as js]))

;;; Event Management

;; Todo: Perhaps event listeners are better than inline events

(defn- merge-attrs [map1 map2]
  (cond (not map1) map2
        (not map2) map1
        :else (do (assert (map? map1) "Programming Error: Trying to merge incompatible html attribute sets.")
                  (assert (map? map2) "Programming Error: Trying to merge incompatible html attribute sets.")
                  ; Todo: Two js/jsq can be merged with a (do (jsi ..) (jsi ..)) 
                  (merge map1
                         (into {} (map #(vector % (merge-attrs (map1 %) (map2 %))) (keys map2)))))))

(defn attributes-into-node [[tag first-or-attrs & node-rest] more-attrs]
  (let [[attrs first] (if (map? first-or-attrs)
                        [first-or-attrs nil]
                        [nil first-or-attrs])]    ; Hiccup will ignore nil
    (vector tag (merge-attrs attrs more-attrs) first node-rest)))

(comment
  (merge-attrs {:a 1} {:b 2})
  (merge-attrs {:a 1} {:a 2}) ; should raise an exception
  (merge-attrs {:a 1 :b {:c 2}} {:b {:d 3}})
  (merge-attrs {:a 1 :b {:c 2}} {:b {:c 3}}) ; should raise an exception
  (attributes-into-node [:p {:a 1 :b {:c 2}}] {:b {:d 3}}))

(defn load-event-handling []
  [:script (js/js '(do (set! actions (new Object))
                       (set! react (fn [reaction ...args]
                                     ((aget reaction 1)
                                      (document.getElementById (aget reaction 0))
                                      ...args)))
                       (set! fire (fn [event ...args]
                                    (doseq
                                     (aget actions event)
                                      (fn [node]
                                        (react node ...args)))))))])

(comment (load-event-handling))

(defn register-event [event]
  [:script (js/jsq (set! (aget actions (uq event))
                         []))])

(comment (register-event "ev"))


(defn register-handler [event id handler]
  [:script (js/jsq (.push (aget actions (uq event))
                          [(uq id) (jsi (uq handler))]))])

(comment
  (let [handler "function(){}"]
    (register-handler "ev" "id-2" handler)))

(defmacro let-events [events & body]
  (let [let-bindings (map #(vector % `(-> ~(name %) (str "_") gensym name)) events)
        init (map #(list `register-event (first %)) let-bindings)]
    `(let ~(into [] (apply concat let-bindings))
       ~(apply list 'list (concat init body)))))

(defmacro let-event-map [[bindvar keys] & body]
  `(let [keys--scripts-events# (map #(vector % (let-events [event#] event#)) ~keys)]
     (let [~bindvar (into {} (map #(vector (first %) (-> % second last)) keys--scripts-events#))]
       (list (map #(-> % second first) keys--scripts-events#) ~@body))))

(comment
  (macroexpand-1 '(let-events [about-window-hide about-window-show] [:div]))

  (let-events [about-window-hide about-window-show] [:div])

  (let-events [about-window-hide about-window-show]
              about-window-hide
              about-window-show)

  (map #(let-events [do-something]
                    do-something %) [1 2])
  
  (let-event-map [events '[a {:a b} 34]]
                 (events 'a)
                 (events '{:a b})
                 (events 34)))

(defn add-id [node id]
  (attributes-into-node node {:id id}))

(comment
  (add-id [:p "a" "b"] "xy1")
  (add-id [:p {:id ""} "a" "b"] "xy1") ; should raise an error
  (add-id [:p {:a :b} "a" "b"] "xy1"))

(defn create-with-event-handler [node & reactions]
  (let [id (gensym)
        script (map #(register-handler (first %) (str id) (second %))
                    reactions)
        node (add-id node id)] ; todo: Probably the id must come out of the node and out of add-id.
    (list node script)))       ; todo: Probably the script must go inside.

(comment
  (create-with-event-handler [:p "node"] '("show" "function(){show()}")
                             '("hide" "function(){hide()}"))
  (create-with-event-handler [:p {:style false}] (list "show" "function(){}")))

(defmacro with-event-handler<-jsq [node & reactions]
  (let [reactions (map #(list `list (first %)
                              (list `js/jsq (list 'fn (into [] (second %)) (last %))))
                       reactions)]
    `(create-with-event-handler ~node ~@reactions)))

(comment
  (macroexpand-1 '(with-event-handler<-jsq [:p "This will disappear"]
                    ("show" [node] (set! node.style.display "none"))))
  (macroexpand-1 '(with-event-handler<-jsq [:p attrs "This will disappear"]
                    ("show" [node] (set! node.style.display "none"))))
  (with-event-handler<-jsq [:p "This will disappear"]
    ("show" [node] (set! node.style.display "none")))
  (with-event-handler<-jsq [:p {:onClick "nothing"} "This will disappear"]
    ("show" [node] (set! node.style.display "none")))
  (let [attrs {:onClick "nothing"}]
    (with-event-handler<-jsq [:p attrs "This will disappear"]
      ("show" [node] (set! node.style.display "auto"))
      ("hide" [node] (set! node.style.display "none"))))
  (let-events [about-window-hide about-window-show]
              [:div (with-event-handler<-jsq [:p "This will disappear"]
                      (about-window-hide [node] (set! node.style.display "none")))
               [:botton]])
  (map #(with-event-handler<-jsq [:p "This will disappear" %]
          ("show" [node] (set! node.style.display "none"))) [1 2]))

(defn fire [event & args]
  (let [command (into args (list event 'fire))]
    (js/js command)))

(comment
  (repeat 2
          (let-events [about-window-hide about-window-show]
                      [:div
                       (with-event-handler<-jsq [:p "This will disappear"]
                         (about-window-hide [node] (set! node.style.display "none"))
                         (about-window-show [node] (set! node.style.display "initial")))
                       [:botton {:onclick (fire about-window-hide)}]
                       [:botton {:onclick (fire about-window-show)}]
                       [:botton {:onclick (fire about-window-show "xy")}]
                       [:botton {:onclick (fire about-window-show (+ 1 1))}]
                       (let [msg "Hello World!"]
                         #_"There are 5 these options to fire events:"
                         (list [:botton {:onclick (fire about-window-show (js/jsq (jsi (.toUpperCase (uq msg)))))}]       ; data from server on client
                               [:botton {:onclick (fire about-window-show "here")}]                                       ; constant
                               [:botton {:onclick (fire about-window-show (.toUpperCase msg))}]                           ; data from server on server
                               [:botton {:onclick (fire about-window-show '(.toUpperCase msg))}]                          ; data from client on client
                               [:botton {:onclick (fire about-window-show (js/jsq (jsi (.toUpperCase msg))))}]))]))       ; data from client on client

  (let-events [about-window-hide]
              [:div
               (with-event-handler<-jsq [:p "This will disappear"]
                 (about-window-hide [node arg] (set! node.style.display arg)))
               [:botton {:onclick (fire about-window-hide "show")}]]))

(defn construct-events-attrs [event-spec]
  (update-vals event-spec
               (fn [event-spec]
                 (->> (if (vector? event-spec) event-spec [event-spec])
                      (map #(if (vector? %) % [%]))
                      (map #(list 'jsi (apply fire %)))
                      (apply list 'do)
                      js/js))))

(comment
  (construct-events-attrs {:onClick "click"
                           :onMouseEnter [["hover" "enter"] "hoverall"]}))

(defn add-events [node event-map]
  (attributes-into-node node (construct-events-attrs event-map)))

(comment
  (let-events [click hover seen]
              (add-events [:button {:border 1} "click-me"]
                          {:onClick click
                           :onMouseEnter [[hover "enter"] seen]
                           :onMouseLeave [[hover "leave"]]})) 
  )

(defn puppet [node & variables-values-events]
  (let [initial-values (->> variables-values-events
                            (filter #(string? (second %)))
                            (map #(list 'set! (first %) (second %)))
                            (apply list 'do)
                            js/js)
        handler (reduce (fn [coll [target & values-events]]
                          (into coll
                                (reduce (fn [coll [value events]]
                                          (into coll
                                                (map (fn [event] (list event (js/jsq (fn [node] (set! (uq target) (uq value))))))
                                                     events)))
                                        [] (last values-events))))
                        [] variables-values-events)] 
    (apply create-with-event-handler (attributes-into-node node {:onLoad initial-values}) handler)))

(comment
  (puppet [:h "node"]
          ['node.style.display "none" [["none" ["ev-f" "ev-g"]]
                                       ["auto" ["ev-h"]]]]
          ['node.visibility "hidden" [["hidden" ["ev-j"]]
                                      ["visible" ["ev-j"]]]])
  
  
  ; The following is not a good programming style. We would not pass these keywords around, because
  ; they represent low level details. But that example demonstrates the whole event mechanism.
  (let-events [cancel]
              (let-event-map [events [:onClick :onMouseEnter :onMouseLeave]]
                             (add-events [:p "I send events"] events)
                             (puppet [:p "I react"] 
                                     ['node.style.display "auto" [["block" [(events :onClick)]]
                                                                  ["auto" [cancel]]]]
                                     ['node.style.visibility "visible" [["hidden" [(events :onMouseEnter)]] 
                                                                        ["visible" [(events :onMouseLeave)]]]]))))

;;; Todo: puppet and with-event-handler<-jsq cannot be used together, because they both create
;;; lists. But both need nodes. They also both add their own id. So this conflicts. Perhaps the
;;;;scripts can be moved inside. Then it can remain nodes. With a simple add-events it works already.