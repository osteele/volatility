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

## License

MIT
