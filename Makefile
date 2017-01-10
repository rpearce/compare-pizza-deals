run:
	elm-make src/Main.elm --output elm.js
	./node_modules/.bin/uglifyjs elm.js --mangle --screw-ie8 -o elm.js -c dead_code -c unused
