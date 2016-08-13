#' Constructor (factory) of statistical models
#'
#' This is a virtual Reference Class for concrete factory RCs
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any factory RC must have.
#'
#' @include v-basic.R
#' @importFrom methods new
#' @exportClass rcvirtual.factory
#'
setRefClass(
    Class = "rcvirtual.factory",
    contains = c("rcvirtual.basic", "VIRTUAL")
)
