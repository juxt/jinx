.PHONY: 		test watch

test:
	clj -Atest


watch:
	find . -regex ".*\\.clj[cs]?" | entr make test
