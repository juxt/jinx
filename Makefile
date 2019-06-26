.PHONY: 		test watch

test:
	clj -Atest -e :deprecated

official-test:
	clj -Atest -i :official

watch:
	find . -regex ".*\\.clj[cs]?" | entr make official-test
