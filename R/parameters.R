#' Constants, random variables and vectors (parameters) of statistical models
#'
#' This is a virtual Reference Class for concrete parameter RCs
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any parameter RC must have.
#'
#' @exportClass rcvirtual.parameters
#'
setRefClass(
        Class = "rcvirtual.parameters",
        contains = c("rcvirtual.basic", "VIRTUAL")
)
