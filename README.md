# Re-state
`Re-state` is a Clojure(Script) library in development for computing numerically accurate and reproducible mechanical responses of systems with shared. The core algorithm code is completely shared providing more extensive testing and easier implementation of new problems. Currently, there is support for solving non-linear particle and rigid body dynamic systems.

See a demo of this library in action [here](https://brianchevalier.github.io/re-state/index.html).

## Why Clojure?
Clojure is a [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)) dialect and [functional programming language](https://en.wikipedia.org/wiki/Functional_programming) with a rich set of persistent data structures. Immutable data structures allow for easier to reason about systems, as well as [concurrency](https://clojure.org/about/concurrent_programming).

In addition, Clojure can be run in a Java runtime or in a JavaScript runtime. This means code can be shared across a high performance runtime and in a standard browser for serverless demos.

# Development
Run the following at a terminal in the root directory to get started.

```
make dev
```

### Project Structure
The user interface components are available under `src/app`. The dynamic specific solving code is available under `src/dynamic`. Some math helper functions are available under `src/math`.

### Technologies
* [React](https://reactjs.org) via [Reagent](https://reagent-project.github.io) for the user interface
* [Vega-Lite](https://vega.github.io/vega-lite/) via [Oz](https://github.com/metasoarous/oz) for plot visualizations
* [math.js](https://mathjs.org) via [clojure.core.matrix](https://github.com/mikera/core.matrix) for linear algebra
* [ClojureScript](https://clojurescript.org) for compiling to JavaScript

# Contributing
* File issues/bugs [here](https://github.com/BrianChevalier/re-state/issues)
* Have some Equations of Motions you'd like to see in action? I'd be happy to add them to the list of examples! Add an issue to get started