.PHONY: dev

node_modules: package.json package-lock.json
	npm ci

index:
	bb -cp src -m app.index > resources/index.html

dev: node_modules index
	clojure -M:core-matrix:dev:cljs:shadow-cljs watch app

resources/main.js:
	clojure -M:cljs:shadow-cljs release client

release: node_modules resources/main.js

demo: release
	bash build-demo

testdemo: demo
	git checkout gh-pages
	open http://0.0.0.0:8000/
	python -m http.server 8000
	git checkout main

deploy: demo
	git checkout gh-pages
	git push --set-upstream origin gh-pages --force
	git checkout main