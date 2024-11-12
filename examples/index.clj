(ns table-examlple
  (:require [hiccup.page :as p]
            [hiccup2.core :as h2c]
            [hTMLkit.html :as hk]
            [hTMLkit.js :as js]))

(defmacro example [& code]
  [:p]
  `(list
    [:p "Example:"]
    [:blockquote [:pre "\n" (list ~@(map #(with-out-str (clojure.pprint/pprint %)) code))]]
    [:p "Result:"]
    [:blockquote (list ~@code)]))

(defmacro define [& code]
  [:p]
  `(list
    [:p "Define:"]
    [:blockquote [:pre "\n" (list ~@(map #(with-out-str (clojure.pprint/pprint %)) code))]] 
    (and (list ~@code) nil)))

(def index
  (list
   [:style "pre { background-color : lightgrey;}"]

   [:h1 "Basic HTML functions"]
;------------------------------

   [:p "We can create normal HTML nodes with Hiccup."]
   (example
    [:p {:style {:background-color "green"}} "Black on Green"])
   [:hr]
   [:p "We can retroactively add more attributes into HTML nodes."]
   (example
    (hk/attributes-into-node
     [:p {:style {:background-color "green"}} "Orange on Green"]
     {:style {:color "orange"}}))
   [:hr]
   [:p "There is a shortcut for the id attribute."]
   (example
    (hk/add-id "node-234"
               [:p {:style {:background-color "green"}} "Black on Green"]))


   [:h1 "Integrating JavaScript Code"]
;-------------------------------------

   [:p "JavaScript can be integrated in two ways. The function js/js takes an expression in the Clojure syntax for JavaScript and renders it into a string "
    "that can be used in Hiccup. The expression must be quoted. Otherwise it would be evaluated before passed to js/js. The convenience macro js/jsq "
    "does the same, but does not require the expression to be quoted."]
   (example
    [:button {:onClick (js/jsq (alert "clicked"))} "click me"])
   (example
    [:button {:onClick (js/js '(alert "clicked"))} "click me"])
   [:p "When writing JavaScript code by hand, js/jsq is more comfortable."]
   [:hr]
   [:p "Let's see a few examples how JavaScript code is generated for us."]
   (example
    (js/jsq (do
              (set! set_s (fn [new_value] (set! x new_value)))
              (set! (.-member_a obj_1) (+ (.-member_b obj_2) u))
              (set! (aget arr_1 3) {:height 32 :width x})
              (console.log "done")
              (console.log :done))))
   [:p "In the body of js/jsq we are in the JavaScript world. Everything including symbols that are unknown in Clojure are interpreted as "
    "JavaScript. There is no syntax or semantic check."]
   [:hr]
   [:p "Often we want to escape the JavaScript world and fill in Clojure Data. This can be done with the uq symbol."]
   (example
    (let [msg "Hello World!"]
      (js/jsq (alert (uq msg)))))
   [:p "This works for data, that can be directly introduced into the JavaScript code."]
   [:hr]
   [:p "Often we also want to introduce JavaScript code into other JavaScript code. Such JavaScript snippets cannot be created with js/jsq. The "
    "only purpose of js/jsq is to create strings for Hiccup. "
    "If we use it to introduce code into code, we would just get a JavaScript string inside the JavaScript code. We would
     have to call eval on it (which is also available in JavaScript). This is not something we normally want."]
   [:p "In order to directly introduce code into code we can use the macro js/q. That behaves in the same way as js/jsq "
    "and also has the uq mechanism to inject data. But unlike js/jsq the macro js/q does not render the code into a string."]
   (example
    (let [author "Sarah"
          msg (js/q (.toUpperCase (uq (str "Hello " author "!"))))]
      (js/jsq (alert (uq msg)))))
   [:hr]
   [:p "An alternative to this is to just quote the code that we want to introduce."]
   (example
    (let [msg '(.toUpperCase "Hello You!")]
      (js/jsq (alert (uq msg)))))
   [:p "However, that is not often practicable, because in this situation there is no unquote. So we cannot introduce data. The "
    "code can only ever be a constant."]
   [:hr]
   [:p "But sometimes we are in the situation that we already have JavaScript code as a s string and we want to introduce that into other "
    "JavaScript code. Normally js/jsq would place quotes around strings. We must prevent this if we want to introduce it as code. That can be done with jsi "
    "around uq."]
   (example
    (let [gen-msg "'Hello You!.toUpperCase()'"]
      (js/jsq (alert (jsi (uq gen-msg))))))
   [:p "Note that (uq (jsi (js/jsq ...))) has the same effect than (uq (js/q ...))"]
   [:p "Note that uq and jsi are not Clojure symbols. They exist only in the JavaScript world and therefore are not namespaced."]
   [:hr]

   [:h2 "Summary and Convention"]
;--------------------------------

   [:p "js/js is a function that takes a Clojure data structure, interprets it as JavaScript code and renders it into a string. As it is a "
    "function, it evaluetes it arguments first. Normally this fails if the code is not quoted."]
   [:p "js/q is a macro that takes a Clojure data structure and returns it without evaluating it. It is similar to the normal Clojure quote, but "
    "in addition has uq, which behaves similar to unquote in Clojure's syntax quote."]
   [:p "js/jsq is a macro that combines js/q with js/js, so that we can comfortably write JavaScript code with the Clojure Syntax, render it "
    "into a string and introduce it into Hiccup."]
   [:p "Many functions of HTMLKit take JavaScript code as a Clojure Data structure, which they introduce into the JavaScript code that they "
    "generate. The user when calling such functions in HTMLKit is responsible for preventing that the code is evaluated."]
   [:p "A few convenience macros take JavaScript code and automatically prevent it from beein evaluated. These convenience macros "
    "in their name end with 'q<-', indicating, that the arguments first are passed into the macro js/q, so that they are not evaluated."]
   [:p "Only a few functions of HTMLKit render JavaScript code into strings. This is the very last step, when the code is introduced into "
    "Hiccup. This is never appropriate when creating and working with JavaScript code. We always use the Clojure syntax. The high level "
    "functions of HTMLKit generate stings for us. But HTMLKit embraces HTML+JS. The user also wants to use JS directly withou the "
    "HTMLKit high level functions."]
   [:hr]
   [:p "We establish a naming for JavaScript instances."]
   [:p "'Unquoted JavaScript' are the arguments that are passed to js/q or js/jsq. Only macros can ever take "
    "unqoted JavaScript as arguments. Functions that take unquoted JavaScript are the most comfortable for the programmers. "
    "But unquoted JavaScript cannot exist at runtime. The reader consumes it by either rendering it into a string or by quoting it."]
   [:p "'Quoted' JavaScript are all data structures at runtime that represent "
    "JavaScript code in the Clojure syntax. There are no limitations when working with quoted JavaScript. Functions can be take quoted JavaScript. "
    "For the programmer it is not comfortable to write quoted JavaScript. Lists have to be manually created and the evaluation must be manually "
    "prevented. But when JavaScript is to be generated programmatically, this must be the preferred way."]
   [:p "'Rendered' JavaScript is the generated "
    "JavaScript code as a string, so that it can be integrated in Hiccup. This is always the very last step."]

   [:h2 "Generating JavaScript"]
;-------------------------------

   [:p "We have now all ingredients to dynamically generate JavaScript code. All functions are easy to understand. But in practice "
    "it is more complicated than expected. "]
   [:p "It is important to understand the difference between js/jsq and js/q. The macro js/q is very similar to quote in Clojure. "
    "It takes an expression and returns that expression without evaluating it. This is what we want when using the Clojure syntax for JavaScript. "
    "Normally we would just use quote. But quote has no unquote. In contrast js/q has unquote, which is is uq. This is the only reason why "
    "HTMLKit does not use just quote. Indeed js/q is a generic mechanism, that has nothing to do with the rest of the js namespace. It can be "
    "used as a spiced up version of quote. HTMLKit is responsible for preventing that the code is evaluated."
    [:p]
    "The macro js/jsq is just there for convenience. It is js/q plus an additional function call to js/js, which takes an expression "
    "and converts it into JavaScript."]
   [:p "When manually writing JavaScript code, the macro js/jsq is comfortable. But js/jsq has downsides. Clojure developers are often not "
    "aware that macros are very limited and should be avoided. They are not first class citicens in Clojure. They cannot be used in "
    "dynamic situations."]
   [:hr]
   [:p " Imagine you have a list of strings in Clojure and you want to create JavaScript that prints them all "
    "out. The obvious way to do this is the following."]
   (example
    (let [audience ["Peter" "Franc" "Joe"]]
      (js/jsq (do* (uq (map (fn [name] (js/q (console.log (uq name)))) audience))))))
   [:hr]
   [:p "Now we don't want to use the constant strings, but JavaScript code for the audience."]
   (example
    (let [audience-code [(js/q (.getFullName db "Peter")) (js/q (.getFullName db "Franc")) (js/q (.getFullName db "Joe"))]]
      (js/jsq (do (console.log (uq (nth audience-code 0))) (console.log (uq (nth audience-code 1))) (console.log (uq (nth audience-code 2)))))))
   [:hr]
    [:p "But in some situations, that are not selden, the macros js/jsq or js/q for programmatically creating JavaScript code do not work. The following example is incorrect."]
   (define
    (defmacro get-full-name<-q [name]
      (let [var-name `fullname#]
        (list 'quote (js/q (do (set! (uq var-name) (.getFullName db (uq name)))
                               (console.log (uq var-name))))))))
   (example
    (let [audience ["Peter" "Franc" "Joe"]
          audience-code (map (fn [firstname] (get-full-name<-q firstname)) audience)]
      (js/jsq (do* (uq audience-code)))))
   [:p "As can be seen there are numerous problems with this macro apart from not beeing easy to understand. "
    "The first problem is that the macro does not take the value for firstname. The "
    "second problem is that all variables, that are meant to be unique have the same name. With macros "
    "there is no workaround for these problems."]
   [:p "The same experiment with a function instead of a macro works as expected."]
   (define
    (defn get-full-name [name]
      (let [var-name (gensym)]
        (js/q (do (set! (uq var-name) (.getFullName db (uq name)))
                  (console.log (uq var-name)))))))
   (example
    (let [audience ["Peter" "Franc" "Joe"]
          audience-code (map (fn [firstname] (get-full-name firstname)) audience)]
      (js/jsq (do* (uq audience-code)))))

   [:h1 "HTML Event Handling"]
 ;----------------------------

   [:p "We have an HTML even thandling mechanism. First we have to load the event handling mechanism."]
   (example
    (hk/load-event-handling))
   [:hr]
   [:p "Then we can register a new events."]
   (example
    (hk/register-event "set-color"))
   [:p "These two functions yield only a very few lines of js."]
   [:hr]
   [:p "Now we can fire the event from js."]
   (example
    [:button {:onclick (js/jsq (fire "set-color"))} "make green"])
   [:hr]
   [:p "The Macro js/jsq converts js code from a special Clojure syntax into a string, that can directly be used in Hiccup. But we don't have "
    "to write js by hand even if in the Clojure syntax. For firering events there is a function that can also be directly integrated into Hiccup."]
   (example
    [:button {:onclick (hk/fire "set-color")} "make green"])
   [:p "In order to react on the event we have to create a node with an id."]
   (example
    [:p {:id "red-node-1"
         :style {:color "red"}}
     "I start red and become green"])
   [:p "Then we can register a handler. This could be done by js. But that is complicated. The function regiser-handler yields the js code for us."]
   (example
    (hk/register-handler "set-color" "red-node-1" (js/q (fn [node] (set! node.style.color "green")))))
   [:p "The id can also set with add-id. For manually written HTML code that is not so important. But for HTML code that is auto generated"
    " with auto-generated ids that simplifies the code."]
   (example
    (hk/add-id "red-node-2"
               [:p {:style {:color "red"}} "I start red and become green"])
    (hk/register-handler "set-color" "red-node-2"
                         (js/q (fn [node] (set! node.style.color "green")))))
   [:p "This can be done all together. An id is automatically set."]
   (example
    (hk/create-with-event-handler
     [:p {:style {:color "red"}} "I start red and become green"]
     ["set-color" (js/q (fn [node] (set! node.style.color "green")))]))
   [:p "For simplification there is a macro for js part. Then we can use Clojure syntax for js."]
   (example
    (hk/create-with-event-handler<-q
      [:p {:style {:color "red"}} "I start red and become green"]
      ("set-color" [node] (set! node.style.color "green"))))
   [:p "This way of defining events is not practicable. Instead of hard coded strings for events, we want events automatically created. The internal JavaScript names of the "
    "events are not of enterest. Events should be used like normal variables in Clojure. There are comfortable let macros for that purpose. "
    "As Clojure is a functional programming language, a React like unidirectional data flow comes automatically."]
   (example
    (hk/let-events [make-green make-blue]
                   [:button {:onclick (hk/fire make-green)} "make green"]
                   [:button {:onclick (hk/fire make-blue)} "make blue"]
                   (hk/create-with-event-handler<-q [:p "I will be colored."]
                     (make-green [node] (set! node.style.color "green"))
                     (make-blue [node] (set! node.style.color "blue"))))) 
   [:hr]
   [:p "Quite often HTML and events are automatically created from data structures."]
   (example
    (let [beverages {:milk "white" :juice "yellow" :whine "red" :coffee "black" :cocoa "brown"}]
      (hk/let-event-map [pour (keys beverages)]
                        (map (fn [beverage]
                               [:button {:onclick (hk/fire (pour beverage))} beverage])
                             (keys beverages))
                        (map (fn [[drink color]]
                               (hk/create-with-event-handler<-q [:p "glass of " drink]
                                 ((pour drink) [node] (set! node.style.color (uq color)))))
                               beverages))))
   [:hr]
   [:p "As already said, macros are limited. In order to add multiple dynamically created handlers, we would need the apply operator, which does not work for macros. "
    "The use case is very common. It can be achieved by not using the "
    "macro hk/create-with-event-handler<-q but the underlying function hk/create-with-event-handler."]
   (example
    (let [beverages {:milk "white" :juice "yellow" :whine "red" :coffee "black" :cocoa "brown"}]
      (hk/let-event-map [pour (keys beverages)]
                        (map (fn [beverage]
                               [:button {:onclick (hk/fire (pour beverage))} beverage])
                             (keys beverages))
                        (apply hk/create-with-event-handler
                               [:p "Glass of liquid"]
                               (map (fn [[drink color]]
                                      (list (pour drink) (js/q (fn [node] (set! node.style.color (uq color))))))
                                    beverages)))))
   [:p "At first seight, this is not intuitive to understand. But it is without technical detours exactly what we want. We created a list of beverages with colors and for each "
    "of them an event, "
    "which we pass down the DOM. Then for each event we create a button that fires it. Finally we create an HTML node that reacts on all these events with an event handler. "
    "That is an unidirectional top-down data-flow like React. But of course it is the responsibility of the user to establish "
    "the desired data flow. Events can freely passed around."]
   [:hr]

   [:h1 "Sending Events with HTML Event Maps"]
;---------------------------------------------
   
   [:p "HTML already knows many events, for example onload, clicked, onmouseover, onsubmit. It is so common to react on these events, that HTMLKit has a helper for this purpose. "
    "HTMLKit provides a convenience function for this."]
   (example
    (hk/let-events [click hover seen]
                   (hk/add-events [:button {:border 1} "click or hover me"]
                                  {:onClick click
                                   :onMouseEnter hover
                                   :onMouseLeave seen})
                   (hk/create-with-event-handler<-q [:p "I will be colored."]
                     (click [node] (set! node.style.color "green"))
                     (hover [node] (set! node.style.color "blue"))
                     (seen [node] (set! node.style.color "lightgrey")))))
   [:p "hk/add-events also supports multiple events."]
   (example
    (hk/let-events [paint-x-blue paint-y-blue]
                   (hk/add-events [:button {:border 1} "click me"]
                                  {:onClick [paint-x-blue paint-y-blue]})
                   [:p
                    (hk/create-with-event-handler<-q [:span "X"]
                      (paint-x-blue [node] (set! node.style.color "blue")))
                    (hk/create-with-event-handler<-q [:span "Y"]
                      (paint-y-blue [node] (set! node.style.color "blue")))])) 
   [:h1 "Events with arguments"]
;-------------------------------
   
   [:p "Events can also have arguments. The functions hk/fire, hk/add-events and hk/create-with-event-handler<-q support "
    "this. The arguments can come from the client or from the server. The following demonstrates all combinations."]
   (example
    (hk/let-events [click]
                   (list [:p (hk/add-events [:button {:border 1} "click me"]
                                            {:onClick [click]})]
                         (hk/create-with-event-handler<-q [:span "The color does not travel in the event. It is determined on the server."]
                           (click [node] (set! node.style.color "blue"))))))

   (example
    (let [input-field-id (str (gensym))]
      (hk/let-events [click]
                     (list [:p "Color:" [:input {:type "text" :id input-field-id}]
                            (hk/add-events [:button {:border 1} "click me"]
                                           {:onClick [click]})]
                           (hk/create-with-event-handler<-q [:span "The color does not travel in the event. It is determined by the event handler."]
                             (click [node] (set! node.style.color (.-value (document.getElementById (uq input-field-id))))))))))

   (example
    (hk/let-events [click]
                   (list [:p (hk/add-events [:button {:border 1} "click me"]
                                            {:onClick [[click "green"]]})]
                         (hk/create-with-event-handler<-q [:span "The color comes through the event. It is determined on the server."]
                           (click [node color] (set! node.style.color color))))))

   (example
    (let [input-field-id (str (gensym))]
      (hk/let-events [click]
                     (list [:p "Color:" [:input {:type "text" :id input-field-id}]
                            (hk/add-events [:button {:border 1} "click me"]
                                           {:onClick [[click (js/q (.-value (document.getElementById (uq input-field-id))))]]})]
                           (hk/create-with-event-handler<-q [:span "The color comes through the event. It is determined where the event is sent. We manually "
                                                      "have to use js/q. HTMLKit never directly takes automatically js if not clear from the"
                                                      "function or macro name. HTMLKit wants to enable TMLKit is responsible for preveting that the code is evaluated.us
                                                      [:p] to avoid direct js."]
                             (click [node color] (set! node.style.color color)))))))))






(->> index
     p/html5 str (spit "examples/index.html"))


(generate-audience-code "asdf")

(def raw-event
  (list [:p "There is a combined let-macro to introduce events."]
        (hk/load-event-handling)
        [:p "Then we can register events."]
        (hk/register-event "set-color")
        [:p "These two functions yield only a very few lines of js. Then we can fire the event from js."]
        [:button {:onclick (js/jsq (fire "set-color"))} "make green"]
        [:p "Create a node with an id"]
        (hk/add-id "red-node" [:p {:style {:color "red"}} "I start red and become green"])
        [:p "Add an event handler"]
        (hk/register-handler "set-color" "red-node" (js/jsq (fn [node] (set! node.style.color "green"))))
        [:p "The last 3 steps can be done all together"]
        (hk/create-with-event-handler
         [:p {:style {:color "red"}} "I start red and become green"]
         ["set-color" (js/jsq (fn [node] (set! node.style.color "green")))])
        [:p "For simplification there is a macro for the js part."]
        (hk/create-with-event-handler<-q
          [:p {:style {:color "red"}} "I start red and become green"]
          ("set-color" [node] (set! node.style.color "green")))
        (hk/create-with-event-handler<-q
          [:p {:style {:color "red"}} "I start red and become green"]
          ["set-color" [node] (set! node.style.color "green")])))

(->> raw-event
     p/html5 str (spit "examples/raw-events.html"))

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
                  [:button {:onClick (hk/fire reset)} "reset"])])

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
                                                           [["orange" "lightgrey"] [select] :keep]
                                                           [:kept    [reset]]
                                                           ["lightgrey" [] :init]]])
                  [:button {:onClick (hk/fire red)} "red"]
                  [:button {:onClick (hk/fire green)} "green"])])

(comment
  (->> (example-1)
       p/html5 str (spit "/tmp/test.html"))
  (->> (example-2)
       p/html5 str (spit "/tmp/test.html")))
