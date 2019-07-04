# Authoritative build rules for json-schema.

# If you don't have GNU Make on your system, use this file as a
# cribsheet for how to build various aspects of jsonschema.

STYLESDIR = ../asciidoctor-stylesheet-factory/stylesheets
STYLESHEET = juxt.css

.PHONY: 		watch default deploy test 

official-test:
	clj -Atest -i :official

test-clj:
			clojure -Atest -e deprecated
test-cljs:
			rm -rf cljs-test-runner-out && mkdir -p cljs-test-runner-out/gen && clojure -Sverbose -Atest-cljs

test:
			make test-clj && make test-cljs

watch:
	find . -regex ".*\\.clj[cs]?" | entr make test

pom:
			rm pom.xml; clojure -Spom; echo "Now use git diff to add back in the non-generated bits of pom"
# Dev pom is used to created development project with intellij
dev-pom:
			rm pom.xml && clojure -R:dev:dev-rebel:dev-nrepl:test-cljs -C:dev:dev-rebel:dev-nrepl:test-cljs -Spom

deploy:			pom
			mvn deploy
figwheel:
			clojure -R:dev:dev-nrepl:dev-rebel -C:dev:dev-nrepl:dev-rebel:test -m figwheel.main --build jsonschema --repl

# hooray for stackoverflow
.PHONY: list
list:
		@$(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$' | xargs