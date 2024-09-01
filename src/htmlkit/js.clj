(ns htmlkit.js 
  #_(:require [clojure.walk :as walk]))

(def js)
(defn jscall [function & arguments]
  (str (js function) "(" (clojure.string/join "," (map js arguments)) ")"))

(defn jsmethodcall [method & arguments]
  (str (js (first arguments)) method "(" (clojure.string/join "," (map js (rest arguments))) ")"))

(defn js [code]
  (cond (fn? code)      (code)
        (number? code)  (str code)
        (symbol? code)  (str code)
        (string? code)  (str "'" code "'")
        (keyword? code) (str "'" (name code) "'")
        (vector? code)  (str "[" (clojure.string/join "," (map js code)) "]")
        (map? code) (str "{" (clojure.string/join "," (map #(str (js %1) ":" (js %2)) (keys code) (vals code))) "}")
        (and (seq? code) (= (first code) 'js<-))
        (second code)
        (seq? code)     (let [operation (str (first code))
                              js-operation (->> operation (str "js") symbol (ns-resolve 'htmlkit.js))]
                          (if js-operation
                            (apply js-operation (rest code))
                            (if (clojure.string/starts-with? operation ".")
                              (apply jsmethodcall code)
                              (apply jscall code))))))

(defn jsjsi [code]
  code)

(comment (js 3)
         (js 3.0)
         (js 0.3)
         (js -1)
         (js -1.1)
         (js "")
         (js "x")
         (js :x)
         (js 'lkj)
         (js '(+ 1 1))
         (js '(+ 1 (+ 2 3)))
         (js '{})
         (js '{a 3})
         (js '{:a 3})
         (js '{"a" 3})
         (js '(js<- "inc(a)"))
         (js '(do (inc a) (js<- "inc(a)")))
         (js '[])
         (js '[a])
         (js '[a b])
         (js '[a "b" :c])
         (js '[a (+ 1 2) c]))

(defn js-block [statements]
  (clojure.string/join ";" (map js statements)))

(comment (js-block '(a))
         (js-block '(a b)))

(defn js+ [a b]
  (str "(" (js a) "+" (js b) ")"))

(defn js- [a b]
  (str "(" (js a) "-" (js b) ")"))

(defn jsset! [from to]
  (str (js from) "=" (js to)))

(defn jsdefn [name arguments & body]
  (str "function " name "(" (clojure.string/join "," (map str arguments)) "){" (js-block body) "}"))

(defn jsfn [arguments & body]
  (str "function" "(" (clojure.string/join "," (map str arguments)) "){" (js-block body) "}"))

(defn jsaget [array idx]
  (str (js array) "[" (js idx) "]"))

(defn jsnew [class]
  (str "new " (str class) "()"))

(defn jsdoseq [seq-exprs body]
  (str (js seq-exprs) ".forEach(" (js body) ")"))

(defn jsasync [func]
  (str "async " (js func)))

(defn jsawait [func]
  (str "await " (js func)))

(defn jsreturn [func]
  (str "return " (js func)))

(defn jsif
  ([condition then]
   (str "if" (js condition) "{" (js then) "}"))
  ([condition then else]
   (str (jsif condition then) (if else (str "else{" (js else) "}"))))) ; don't know if parenthesis required.

(defn js= [left right]
  (str "(" (js left) "==" (js right) ")"))

(defn js!= [left right]
  (str "(" (js left) "!=" (js right) ")"))

(comment (js '(func))
         (js '(func arg_1 arg_2))
         (js '(func_1 (func_2 1)))
         (js '(method_1 o))
         (js '(.method_1 o a1 a2))
         (js '(.method_1 o (func_2 1)))
         (js '(+ 1 1))
         (js '(- 1 1))
         (js '(set! a b))
         (js '(set! a (+ 3 b)))
         (js '(set! (a 16) b))
         (js '(defn func_1 [] (set! a 4)))
         (js '(defn func_1 [b] (set! a b)))
         (js '(defn func_1 [b c] (set! a (+ b c))))
         (js '(defn func_1 [] (set! a 16) (set! b 12)))
         (js '(set! func_1 (fn [] (set! a 4))))
         (js '(set! func_1 (fn [b] (set! a b))))
         (js '(set! func_1 (fn [b c] (set! a (+ b c)))))
         (js '(set! func_1 (fn [] (set! a 16) (set! b 12)))) 
         (js '(fn [] (set! a 4)))
         (js '(fn [b] (set! a b)))
         (js '(fn [b c] (set! a (+ b c))))
         (js '(fn [] (set! a 16) (set! b 12)))
         (js '(aget a 3))
         (js '(set! (aget a 3) 17))
         (js '(new Car))
         (js '(= a b))
         (js '(!= a b))
         (js '(if (= a 2) b))
         (js '(if (= a 2) b c))
         )

(defn jsdo [& code]
  (js-block code))

(comment (js '(+ a b))
         (js '(inc a))
         (js '(do (inc a)
                  (inc b)))
         (js '(do (inc a))))

#_(defmacro qu 
  [code]
  (cond (symbol? code)                            (list 'quote code)
        (number? code)                            code
        (string? code)                            code
        (keyword? code)                           code
        (vector? code)                            (apply vector (map #(list `qu %) code))
        (map? code)                               (update-keys (update-vals code #(list `qu %)) #(list `qu %))
        (and (list? code) (= (first code) 'uq)) `(fn [] ~(second code))
        (and (list? code))                        (apply list 'list (map #(list `qu %) code))))


(defmacro q
  [code]
  (cond (symbol? code)                            (list 'quote code)
        (number? code)                            code
        (string? code)                            code
        (keyword? code)                           code
        (vector? code)                            (apply vector (map #(list `q %) code))
        (map? code)                               (update-keys (update-vals code #(list `q %)) #(list `q %))
        (and (list? code) (= (first code) 'uq))   (second code)
        (and (list? code))                        (apply list 'list (map #(list `q %) code))))


#_ ; This was the first implementation. But the one above also seems to work well.
(defmacro q [code]
  `(clojure.walk/postwalk
    (fn [item#] (if (fn? item#) (item#) item#)) (qu ~code)))

(comment
  (q 4) 
  (q b)
  (q "abc")
  (q ())
  (q [])
  (q (a))
  (q (a b))
  (q ({a b}))
  (q {a b})
  (q [a b c])
  (q (a [b c d] e))
  (q (a (b c d) e))
  (let [x 'b] (q 4))
  (let [x 'b] (q (uq x)))
  (let [x 'b] (q (a (uq x))))
  (let [x 'b] (q ({a (uq x)})))
  (let [x 'b] (q {a (uq x)}))
  (let [x 'b] (q [a (uq x) c]))
  (let [x 'b] (q (a [(uq x) c d] e)))
  (let [x 'b] (q (a ((uq x) c d) e)))
  (let [x 'b] (q ({a (uq x) c d} e)))
  (let [x 'b] (q (uq (map identity ['a (q ((uq x) c))])))) 
  )

(defmacro jsq [code]
  `(js (q ~code)))

(comment

  (jsq (inc x))
  (jsq (a (uq 4)))
  (jsq (a (jsi (uq "get_x()"))))  ;inject own code from a string
  (jsq (a (uq "get_x()")))        ;inject strings
  (jsq (a (uq (str "x"))))        ;inject strings
  (jsq (a (uq (js 4))))           ;makes no sense. Goes without ->js
  (jsq (a (jsi (uq (js 4)))))     ;makes no sense. Goes without ->js
  (jsq (a (jsi (uq (js "x")))))   ;makes no sense. Goes without ->js
  (jsq (a (uq (q (inc x)))))      ;inject code from somehow else
  (jsq (a (uq '(inc x))))         ;inject code snippet
  "In the expression `(jsi (uq 'get_x()'))` jsi is the javascript part of the job and
   uq is the clojure part of the job."

  (jsq (a (uq '(x (+ 1 1)))))     ; Does not work. See next line. 
  (jsq (a (x (uq (+ 1 1)))))

  (jsq (a (uq (map identity '(a b c)) c)))

  (let [call 'func_1]
    (jsq ((uq call) a)))

  (let [lk "License-Key" f "file" rn "RealName" x "get_annotations.xfdf?file=file"]
    (jsq
     (.then (WebViewer {path "WebViewer/lib"
                        licenceKey (uq lk)
                        initialDoc (uq f)}
                       (document.getElementById "viewer"))
            (fn [instance]
              (set! annotation_manager instance.Core.annotationManager)
              (annotation_manager.setCurrentUser (uq rn))
              (instance.Core.documentViewer.setDocumentXFDFRetriever
               (async (fn []
                        (set! response (await (fetch "get_annotations.xfdf?file=file")))
                        (set! xfdf (await (response.text)))
                        (console.log xfdf)
                        (return xfdf))))
              (instance.Core.annotationManager.addEventListener "annotationChanged"
                                                                (async (fn [e]
                                                                         (await (fetch "annotation_changed.html?file=file"
                                                                                       {method "POST"
                                                                                        headers {"Content-Type" "application/xml"}
                                                                                        body (await (instance.Core.annotationManager.exportAnnotationCommand))}))
                                                                         (console.log (instance.Core.annotationManager.exportAnnotations)))))
              (instance.UI.disableElements ["toolbarGroup-Shapes"])
              (instance.UI.setHeaderItems (fn [header]
                                            (set! item (header.getItems))
                                            (set! item (item.slice 2 -1))   ; todo: Better not use the indices here
                                            (header.update item))))))
)
; => "WebViewer({path:\"WebViewer/lib\",licenseKey:\"license-key\",initialDoc:\"file\"},document.getElementById(\"viewer\")).then(function(instance){annotation_manager=instance.Core.annotationManager;annotation_manager.setCurrentUser(\"realname\");instance.Core.documentViewer.setDocumentXFDFRetriever(async function(){response=await fetch(\"get_annotations.xfdf?file=file\");xfdf=await response.text();console.log(xfdf);return xfdf});instance.Core.annotationManager.addEventListener(\"annotationChanged\",async function(e){await fetch(\"annotation_changed.html?file=file\",{method:\"POST\",headers:{\"Content-Type\":\"application/xml\"},body:await instance.Core.annotationManager.exportAnnotationCommand()});console.log(instance.Core.annotationManager.exportAnnotations())});instance.UI.disableElements([\"toolbarGroup-Shapes\"]);instance.UI.setHeaderItems(function(header){item=header.getItems();item=item.slice(2,-1);header.update(item)})})"
  )