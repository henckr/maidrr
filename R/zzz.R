# This is where .onLoad and friends functions go

# Package startup message
.onAttach <- function(libname, pkgname) {
  packageStartupMessage('maidrr: develop a Model-Agnostic Interpretable Data-driven suRRogate.')
}
