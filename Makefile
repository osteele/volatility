.PHONY: build
build:
	@mkdir -p build
	@rm -rf build/assets
	cp -r assets build
	elm make src/Main.elm --output build/index.html
	sed 's/\(<head>\)/\1<meta name="viewport" content="width=device-width, initial-scale=1.0">/' -i build/index.html

.PHONY: deploy
deploy: build
	netlify deploy
