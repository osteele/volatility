deploy:
	@mkdir -p build
	elm make src/Main.elm --output build/index.html
	netlify deploy
