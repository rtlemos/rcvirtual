#' Controller (daemon) of statistical models
#'
#' This is a virtual Reference Class for concrete daemon RCs
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any daemon RC must have.
#'
#' @exportClass rcvirtual.daemon
#'
setRefClass(
    Class = "rcvirtual.daemon",
    contains = c("rcvirtual.basic", "VIRTUAL")
)
