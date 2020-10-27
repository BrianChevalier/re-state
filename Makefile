node_modules:
	npm ci

dev: node_modules
	clojure -M:core-matrix:cljs:shadow-cljs watch app