.PHONY: dev

node_modules: package.json package-lock.json
	npm ci

dev: node_modules
	clojure -M:core-matrix:dev:cljs:shadow-cljs watch app

resources/main.js:
	clojure -M:cljs:shadow-cljs release client

release: node_modules resources/main.js

demo: release
	bash build-demo