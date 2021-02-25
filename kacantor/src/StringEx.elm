module StringEx exposing (prepend)


prepend : String -> String -> String
prepend base prefix =
    String.append prefix base
