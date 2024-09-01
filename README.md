# Purpoes

The purpose of HTMLKit is

1. A Clojure syntax for JS
2. HTML event handling

.

This library is useful when you have server based rendering. Typical use cases are web pages that visualise something and let the user highlight, hide, filter information. In these use cases the state is typically not a challenging issue. HTMLKit does not address this.

Inappropriate use cases are web pages that edit something.

# Unique Selling Points

Compared to ClojureScript or libraries like Squint, HTMLKit does not compile Clojure code into JS. HTMLKit is only a simple syntax conversion, so that JS can easier be created dynamically and integrated into Hiccup. If you use htmlkit/js, you directly write JS with all the semantics of JS and with mutable data. It does not only feel like writing JS. It is writing JS, but in a more homoiconic way.

Compared to Scriptjure, the JS part of HTMLKit is less macro based. The main conversion into JS is done by htmlkit/js, which is a function and not a macro. That makes the code easier and the usage more versatile.

The HTML event handling is a simple and naive pure JS based mechanism, that allows to create HTML code with some basic behaviour like hover events, click events, click reactions. The Raison d'Être is to yield out of Hiccup static HTML code with a decent set dynamic behaviour already baked into it, so that no JS library is required. Generated HTML files can be stored as is on the file system and are operational when opened as a static page. In simple cases this could be a good alternative to complex JS frameworks.

The approach differs from HTMX in that point that with HTMLKit there is no central machinery in the control of everything. The HTML code is reactive as is.

HTMLKit goes in a similar direction than Electric, but only a very small step. But Electric is explicitly made for experts while HTMLKit is meant to be kept simple and accessible for less experienced persons.

# Future of HTMLKit

Squint seems to be the ideal solution for the same purpose. It generates small JS code and it even allows to use typical Clojure code, and not only a Clojure syntax for JS. I have not tried it, because as someone without JS expertise and without knowledge of any JS framework it is hard to understand what Squint is and what it does. I don't understand why it uses the JSX syntax if we have Hiccup. I don't understand for what it has a component mechanism. I will try out Squint some day (perhaps when it is more settled) and learn more what it is. I may abandon HTMLKit then if I find Squint more practicable. For the time being I am happy with HTMLKit.

I am convinced that Electric is the right tool for me. But I don't know how practicable Electric in my environment is. Not all persons that I address with HTMLKit are experts enough for Electric, and maybe I am not either. Web development is not a hobby and not an interesting topic for me. However, I may try out Electric in the future. I may abandon HTMLKit if I find Electric more practicable.

I may plan some basic mechanis for data exchange between frontend and backend.

I may plan a basic mechanism for state management if I find something that is useful without poinsoning the mental DOM. But I cannot change the situation that HTML is stateful. So I will not try to framework away the state. But with Server Side Rendering this exercise is different than with things like React or Redux. We don't have to actively pass the state from the parent nodes to the child nodes. We naturally can use Clojure bindings for that purpose. That will work for everything, which does not affect the state of the application. That will enable simple UI DOM visualisation manipulations kept simple. But it will not enable a real data editing, which I don't address with HTMLKit.

Please let me know if you use HTMLKit. If there are people actually using it, I feel more engaged to it. At the moment it is only a project for my own needs. I will give it up at any time, when I am in the mood to stop it.

# Disclaimer
I have no JS experience. I don't know any JS framework. I don't know ClojureScript. I have not even tried any of these technologies. This is why I claim HTMLKit naive. I treat JS like an DOM modification language, and nothing else. I cannot say, how practicable my approach for typical applications is. I have no real world feedback from anybody, no success stories, no long term vision and don't feel responsible for the concept of HTMLKit. How well this approach scales for more complicated things, I don't know. For my personal use cases it is exactly what I need, and nothing more.

# Compatibility

Whether this library works for ClojureScript or Squint I don't know. I see no use case for that combination.

This library works with Babashka.