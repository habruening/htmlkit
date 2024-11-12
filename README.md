# HTMLKit

Why do we develop the backend differently than the frontend? Why do we use different programming languages for both?

* Because we want to separate the business logic from the representation. HTMLKit has a different answer to this. Programming languages are made for abstraction and separation of concerns. If we want to separate the business logic from the representation we use the concepts that our programming language offers us, and not the boundary between systems, where many more things happen than just a separation of concerns.
* Because we want some things to happen on the client so that we have a better (faster) user experience and performance. HTMLKit has a different answer to this. The need for a quick reaction time on the client is a technical constraint. HTMLKit does this for you.
* Because we want to save computation time on the server. HTMLKit is not about that specific need. If you have to serve millions of clients, HTMLKit is not for you. HTMLKit is for enterprise applications where we have docens of clients but not thousands or millions.

# Motivation

With JavaScript frameworks full applications with a complex machinery in the frontend can be created. Often web pages are not intended to be full applications running on the client. When web pages are meant to be only the representation of something, which comes directly from the server with a minimal set of visualisation effects (e.g. colors, highlighting folding, filtering), JavaScript frameworks go too far. These small visualisations effects or interactivity features can be directly baked into the HTML page.

Another motivation is to generate HTML files, that include a minimal set of visualisation effects without any JavaScript dependencies, so that HTML files can even be stored and used offline. For formal documentation (archivation) purposes offline capability is mandatory.

# Purpoes

The purpose of HTMLKit is

* A Clojure syntax for JS
* An HTML event handling
.

HTMLKit targets use cases where HTML pages are to be created server based. Typical use cases are web pages that visualise something and let the user highlight, hide, filter information.

Inappropriate use cases for HTMLKit are more complex web pages, real web applications and everything where not the visualisation but editing is the focus.

# Unique Selling Points

Compared to ClojureScript or libraries like Squint, HTMLKit does not compile Clojure code into JavaScript. HTMLKit is only a simple syntax conversion, so that JavaScript can easier be created dynamically and integrated into Hiccup. If you use htmlkit/js, you directly write JavaScript with all the semantics of JavaScript and with mutable data. It does not only feel like writing JavaScript. It is writing JavaScript, but in a more homoiconic way.

Compared to Scriptjure, the JavaScript part of HTMLKit is not macro based. The conversion into JavaScript is done by htmlkit/js, which is a function and not a macro. That makes the code easier and the usage more versatile.

The HTML event handling is a simple and naive pure JavaScript based mechanism, that allows to create HTML code with some basic behaviour like hover events, click events, click reactions. The Raison d'Être is to yield out of Hiccup static HTML code with a decent set of dynamic behaviour already baked into it, so that no JavaScript library is required. Generated HTML files can be stored as is on the file system and are operational when opened as a static page. In simple cases this could be a good alternative to complex JavaScript frameworks.

The approach differs from HTMX in that point that with HTMLKit there is no central machinery in the control of everything. The HTML code is reactive as is.

HTMLKit goes in a similar direction than Electric, but only a very small step. But Electric is explicitly made for experts while HTMLKit is meant to be kept simple and accessible for less experienced persons.

Svelte is similar to HTMLKit. But two major points distinguish them. In Clojure we can use Hiccup to format HTML. That feels more natural to generate HTML code than Svelte, because we need no component or template mechanism. Simple Function calls give us all we need. The other point of Svelte is, that it is compiled. The Svelte compiler takes so called "Runes" and determines the nesessary JavaScript code for them automatically. That happens magically in a comfortable way for the user. But in practice, this is just a penny. When writing HTML files, it is no effort to explicitly mark the places where updating and re-rendering shall happen.

# Future of HTMLKit

Squint seems to be the ideal solution for the same purpose. It generates small JavaScript code and it even allows to use typical Clojure code, and not only a Clojure syntax for JavaScript. I have not tried it, because as someone without JavaScript expertise and without knowledge of any JavaScript framework it is hard to understand what Squint is and what it does. I don't understand why it uses the JSX syntax if we have Hiccup. I don't understand for what it has a component mechanism. I will try out Squint some day (perhaps when it is more settled) and learn more what it is. I may abandon HTMLKit then if I find Squint more practicable. For the time being I am happy with HTMLKit.

I am convinced that Electric is the right tool for me. But I don't know how practicable Electric in my environment is. Not all persons that I address with HTMLKit are experts enough for Electric, and maybe I am not either. Web development is not a hobby and not an interesting topic for me. However, I may try out Electric in the future. I may abandon HTMLKit if I find Electric more practicable.

I may plan some basic mechanis for data exchange between frontend and backend.

I may plan a basic mechanism for state management if I find something that is useful without poinsoning the mental DOM. But I cannot change the situation that HTML is stateful. So I will not try to framework away the state. But with Server Side Rendering this exercise is different than with things like React or Redux. We don't have to actively pass the state from the parent nodes to the child nodes. We naturally can use Clojure bindings for that purpose. That will work for everything, which does not affect the state of the application. That will enable simple UI DOM visualisation manipulations kept simple. But it will not enable a real data editing, which I don't address with HTMLKit.

Please let me know if you use HTMLKit. If there are people actually using it, I feel more engaged to it. At the moment it is only a project for my own needs. I will give it up at any time, when I am in the mood to stop it.

# Disclaimer
I have no JavaScript experience. I don't know any JavaScript framework. I don't know ClojureScript. I have not even tried any of these technologies. This is why I claim HTMLKit naive. I treat JavaScript like an DOM modification language, and nothing else. I cannot say, how practicable my approach for typical applications is. I have no real world feedback from anybody, no success stories, no long term vision and don't feel responsible for the concept of HTMLKit. How well this approach scales for more complicated things, I don't know. For my personal use cases it is exactly what I need, and nothing more.

# Compatibility

Whether this library works for ClojureScript or Squint I don't know. I see no use case for that combination.

This library works with Babashka.