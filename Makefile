deploy:
	@mkdir -p build
	@rm -rf build/assets
	cp -r assets build
	elm make src/Main.elm --output build/index.html
	netlify deploy
