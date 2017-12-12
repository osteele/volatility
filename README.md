# Coindie

Coindie (coin+die) is a single-page web app that displays the current value of Bitcoin alongside the face of a six-sided die.

## Develop

1. Install [Elm Platform](https://guide.elm-lang.org/install.html). On macOS with Homebrew: `cask install elm-platform`.
2. `elm-reactor`
3. Visit <http://localhost:8000/src/Main.elm>

For live reload:

1. `yarn global add elm elm-live`
2. `elm-live src/Main.elm`
3. Visit <http://localhost:8000>

## Deploy

`elm make src/Main.elm --output index.html`

## Credits

Blockchain info is form the [Blockchain.info](https://blockchain.info) API.

Images:
* Dice vicki4net@pixabay <https://pixabay.com/en/games-die-dice-spot-dot-cube-1693114/>
* Soap bubble <http://maxpixel.freegreatpicture.com/Ease-Float-Background-Soap-Bubbles-Blow-Fly-Water-2670288>

## License

MIT
