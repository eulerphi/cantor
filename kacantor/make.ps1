function deploy ($dir) {
    $path = Join-Path ~\src\eulerphi.github.io $dir
    $buildPath = Join-Path $path "build"

    mkdir $path
    mkdir $buildPath

    Copy-Item .\index.html $path
    Copy-Item .\style.css $path
    Copy-Item .\build\app.js $buildPath
}

if ($args[0] -eq "live") {
    elm-live src/Main.elm --open '--' --output build/app.js --debug
}
elseif ($args[0] -eq "deploy") {
    deploy "kacantor"
}
elseif ($args[0] -eq "deploy-test") {
    deploy "kacantor-test"
}
else {
    elm make src/Main.elm --output build/app.js
}

