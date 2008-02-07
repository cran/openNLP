.onLoad <- function(libname, pkgname) {
    require("methods")
    require("rJava")
    .jpackage(pkgname)
}
