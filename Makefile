node_modules: package.json package-lock.json
	npm ci

dev: node_modules
	clojure -M:core-matrix:dev:cljs:shadow-cljs watch app