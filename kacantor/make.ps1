if ($args[0] -eq "live") {
    elm-live src/Main.elm --open '--' --output build/app.js --debug
}
elseif ($args[0] -eq "deploy") {
    Copy-Item .\index.html ..\..\eulerphi.github.io\kacantor\
    Copy-Item .\style.css ..\..\eulerphi.github.io\kacantor\
    Copy-Item .\build\app.js ..\..\eulerphi.github.io\kacantor\build
}
else {
    elm make src/Main.elm --output build/app.js
}

