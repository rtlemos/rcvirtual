#' Random variables and vectors
#'
#' This is a virtual Reference Class for random variable and random vector RCs
#'
#' @include v-basic.R
#' @importFrom rcrandom rcrng
#' @importFrom methods new
#' @exportClass rcvirtual.random
#'
setRefClass(
  Class = "rcvirtual.random",
  contains = c("rcvirtual.basic", "VIRTUAL"),
  fields = list(type = 'character',
                univariate = "logical",
                lb = 'numeric',
                ub = 'numeric',
                nr = "numeric",
                nc = "numeric",
                size = 'numeric',
                rng = "rcrng"),
  methods = list(

    initialize = function(name = NULL,
                          type,
                          lb = -.Machine$double.xmax,
                          ub = .Machine$double.xmax) {
      if (is.null(name)) {
        .self$object.name <- 'NA'
      } else {
        .self$object.name <- name
      }
      .self$type <- type
      .self$lb <- lb
      .self$ub <- ub
      .self$type <- type
    }
  )
)
