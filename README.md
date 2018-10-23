# flock-reagent


A port of [tzach's flock](https://github.com/tzach/flock) demo to use reagent
for managing application state. Also an experiment in using [stateful components](https://github.com/Day8/re-frame/blob/master/docs/Using-Stateful-JS-Components.md).

### Run application:

```
lein clean
lein figwheel dev
```

Figwheel will automatically push cljs changes to the browser.

Wait a bit, then browse to [http://localhost:3449](http://localhost:3449).

## Production Build


To compile clojurescript to javascript:

```
lein clean
lein cljsbuild once min
```
