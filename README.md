The purpose of HTMLKit is

1. A Clojure syntax for JS
2. HTML event handling

.

Compared to ClojureScript or libraries like Squint, HTMLKit does not compile Clojure code into JS. HTMLKit is only a simple syntax conversion, so that JS can easier be created dynamically and integrated into Hiccup. If you use htmlkit/js, you directly write JS with all the semantic of JS and mutability. It does not only feel like writing JS. It is writing JS, but in a homoiconic way.

Compared to Scriptjure, the JS part of HTMLKit is less macro based. The main conversion into JS is done by htmlkit/js, which is a function and not a macro. That makes the code easier and the usage more versatile.

The HTML event handling is a simple and naive pure JS based mechanism, that allows to create HTML code with some basic behaviour like hover events, click events, click reactions. The raison Raison d'Être is to yield HTML code with a decent set dynamic behaviour directly out of Hiccup, so that no further JS library is required. In simple cases this could be a good alternative to complex JS frameworks. The approach differs from HTMX in that point that with HTMLKit there is no central machinery in the control of everything. The HTML code is reactive as is.

Disclaimer: I have no JS experience. I don't know any JS framework. I don't know ClojureScript. I have not even tried any of these technologies. This is why I claim HTMLKit naive. I treat JS like an DOM modification language, and nothing else. I cannot say, how practicable my approach for typical applications is. I have no real world feedback from anybody, no success stories, no long term vision and don't feel responsible for the concept of HTMLKit. For my personal use cases it is exactly what I need, and nothing more.
