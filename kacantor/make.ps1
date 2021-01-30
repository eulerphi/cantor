if ($args[0] -eq "live") {
    elm-live src/Main.elm --open '--' --output build/app.js --debug
}
else {
    elm make src/Main.elm --output build/app.js
}

