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
                                        (react node ...args)))))
                       (set! othervalue (fn [alternatives current]
                                          (if (= (aget alternatives 0) current)
                                            (return (aget alternatives 1))
                                            (return (aget alternatives 0)))))))])

(comment (load-event-handling))

(defn register-event [event]
  [:script (js/jsq (set! (aget actions (uq event))
                         []))])

(comment (register-event "ev"))

(type (array-map :a 3))
(type {:a 3})
val
(require 'clojure.walk)
(clojure.walk/postwalk (fn [item]
                          (if (string? item)
                            {item (gensym)}
                            item))
                        {:a [{:23 "sdf"}]})

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
  (create-with-event-handler [:p "node"]
                             '("show" "function(){show()}")
                             '("hide" "function(){hide()}"))
  (create-with-event-handler [:p {:style false}] (list "show" "function(){}")))

(js/js '(set! node.style.display "none"))

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
                           :onMouseLeave [[hover "leave"]]})))

(defn- initial-value? [[value events & opts]]
  (some #(= % :init) opts))

(comment
  (initial-value? ["none" [:a :b]])
  (initial-value? ["none" [:a :b] :init])
  (initial-value? ["none" [:a :b] :c :init])
  (initial-value? ["none" [:a :b] :init :d]))

(defn- initial-value [values-events]
  (->> values-events (filter initial-value?) first first))

(comment
  (initial-value [["none" []]])
  (initial-value [["none" []]
                  ["init" [] :init]]))

(defn- initial-values [variables-values-events]
  (->> variables-values-events
       (map #(vector (first %) (initial-value (second %))))))

(comment
  (initial-values [[:a [["none" []]
                        ["init-a" [] :init]]]
                   [:b [["none" []]
                        ["init-b" []]]]
                   [:c [["none" []]
                        ["init-c" [] :init]]]]))

(defn- with-data-ids [initial-values]
  (map #(conj % (clojure.string/lower-case (str (gensym)))) initial-values))

(comment
  (with-data-ids
    (initial-values [[:a [["none" []]
                          ["init-a" [] :init]]]
                     [:b [["none" []]
                          ["init-b" []]]]
                     [:c [["none" []]
                          ["init-c" [] :init]]]])))

(defn- initialisation-attrs [variables-initials-data-ids]
  (->> variables-initials-data-ids
       (filter second)
       (map (fn [[_ init-value data-id]]
              (vector (keyword (str "data-" data-id)) init-value)))
       (into {})))

(comment
  (initialisation-attrs '([:a "init-a" "g__33519"] [:b nil "g__33520"] [:c "init-c" "G__33521"])))

(defn- jsq-setter [target value & {:keys [from keep incase]}]
  (let [new-value (cond (= value :kept) (symbol from)
                        (vector? value) (js/q (othervalue (uq value) (uq keep)))
                        :else value)
        jsq-set-value     (js/q (set! (uq target) (uq new-value)))
        jsq-keep (if keep (js/q (set! (uq keep) (uq new-value))))
        jsq-set (if jsq-keep
                  (js/q (do (uq jsq-set-value) (uq jsq-keep)))
                  jsq-set-value)
        jsq      (if incase 
                   (js/q (if (= (uq incase) (uq target))
                             (uq jsq-set)))
                   jsq-set)] 
    (js/jsq (fn [node] (uq jsq)))))

(js/jsq (if (= (uq 'b) (uq 'a))
          (uq 'c)))

(comment
  (jsq-setter 'node.style.color "red" 'data.attributes.xyz)
  (jsq-setter 'node.style.color :kept :from 'data.attributes.xyz)
  (jsq-setter 'node.style.color "red" :keep 'data.attributes.xyz)
  (jsq-setter 'node.style.color "red" 'data.attributes.xyz :init)
  (jsq-setter 'node.style.color "red" :keep 'data.attributes.xyz :incase "blue"))

(defn reduceme [coll next]
  (cond coll (conj coll next)
        (= next :incase) [next]))

(reduceme [:a] 4)
(conj [3] 4)


(defn search-incase [opts]
  (first (rest (reduce #(cond %1 (conj %1 %2)
                 (= %2 :incase) [%2])
          nil opts))))

(search-incase [1 2 :incase 3])

(defn- as-event-handlers
  ([variables-values-events]
   (mapcat #(apply as-event-handlers %) variables-values-events))
  ([value opts]
   (fn [target data-id]
     (let [dataset-id (symbol (str "node.dataset." data-id))
           keep-in (if (some #(= % :keep) opts) dataset-id)
           incase (search-incase opts)]
       (jsq-setter target value :from dataset-id :keep keep-in :incase incase))))
  ([target values-events data-id]
   (let [events-handlers (fn [[value events & opts]]
                           (map #(vector % ((as-event-handlers value opts) target data-id)) events))]
     (mapcat events-handlers values-events))))

(as-event-handlers 'node.style.color
                   [["none"    ["click" "clock"]]
                    ["default" ["reset"] :keep]
                    [:kept     ["undo"]]]
                   'color)

(as-event-handlers (list ['node.style.display
                          [["none" ["ev-f" "ev-g"]]
                           ["auto" ["ev-h"]]]
                          'asdf]
                         ['node.visibility
                          [["hidden" ["ev-j"]]
                           ["visible" ["ev-j"]]]
                          'sdf]))

(as-event-handlers [['node.style.color
                     [["none"    ["click" "clock"]]
                      ["default" ["reset"] :keep]
                      [:kept     ["undo"]]]
                     'data-color]
                    ['node.style.background
                     [["none"    ["click" "clock"]]
                      ["bg-default"    ["reset"] :keep]
                      [:kept     ["undo"]]]
                     'dg]])

(as-event-handlers [['node.style.color
                     [["none"    ["click" "clock"]]
                      ["default" ["reset"] :keep]
                      [:kept     ["undo"]]]
                     'data-color]
                    ['node.style.background
                     [["none"    ["click" "clock"] :incase "abcd"]
                      ["bg-default"    ["reset"] :keep]
                      [:kept     ["undo"]]]
                     'dg]])

(defn reactive-toggle [node & variables-values-events]
  (let [initial-values (-> variables-values-events initial-values with-data-ids)
        initial-attrs  (initialisation-attrs initial-values)
        handlers       (->> variables-values-events
                            (map #(conj %2 %1) (map last initial-values))
                            as-event-handlers
                            (into []))] 
    (apply create-with-event-handler (attributes-into-node node initial-attrs) handlers)))
         
; Todo: The 'node in reactive-toggle is a magic string. It must be an arg. Not a problem, but
;       inconsistent.

(comment
  (reactive-toggle [:h "node"]
          ['node.style.display [["none" ["ev-f" "ev-g"]]
                                ["auto" ["ev-h"]]]]
          ['node.visibility [["hidden" ["ev-j"]]
                             ["visible" ["ev-j"]]]])
  
  (reactive-toggle [node.style.display [["none" ["ev-f" "ev-g"]]
                                        ["auto" ["ev-h"]]]
                    node.visibility [["hidden" ["ev-j"]]
                                     ["visible" ["ev-j"]]]]
                   [:h "node"])

  (reactive-toggle [:h "node"]
          ['node.style.display [["none" ["ev-f" "ev-g"] :keep]
                                ["inline" ["ev-h"] :init]
                                [:kept ["ev-h" "ev-f"]]]])

  ; The following is not a good programming style. We would not pass these keywords around, because
  ; they represent low level details. But that example demonstrates the whole event mechanism.
  (let-events [cancel]
              (let-event-map [events [:onClick :onMouseEnter :onMouseLeave]]
                             (add-events [:p "I send events"] events)
                             (reactive-toggle [:p "I react"]
                                          ['node.style.display [["block" [(events :onClick)]]
                                                                ["auto" [cancel] :init]]]
                                          ['node.style.visibility [["hidden" [(events :onMouseEnter)]]
                                                                   ["visible" [(events :onMouseLeave)] :init]]]))))

;;; Todo: reactive-toggle and with-event-handler<-jsq cannot be used together, because they both create
;;; lists. But both need nodes. They also both add their own id. So this conflicts. Perhaps the
;;;;scripts can be moved inside. Then it can remain nodes. With a simple add-events it works already.