# This is where .onLoad and friends functions go

# Package startup message
.onAttach <- function(libname, pkgname) {
  packageStartupMessage('Welcome to maidrr!
Let me aid you in developing a Model-Agnostic Interpretable Data-driven suRRogate for your black box.')
}
