#' Uniform random variables
#'
#' This is a Reference Class for scalar Uniform random variables
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any random RC must have.
#'
#' @include d-random.R
#' @importFrom methods new
#' @export Uniform
#' @exportClass Uniform
#'
Uniform <- setRefClass(
  Class = "Uniform",
  contains = c("rcvirtual.random"),
  fields = list(lb = 'numeric',
                ub = 'numeric'),
  methods = list(
    initialize = function(lb = 0, ub = 1, name = NULL) {

      callSuper(name = name, type = 'Uniform', lb = lb, ub = ub)
      .self$size <- length(lb)
      stopifnot(length(lb) == length(ub))
    },

    pdf = function(quantile, log = FALSE) {
      if (quantile < .self$lb | quantile > .self$ub) {
        out <- if(log) -1e10 else 1e-10
      } else {
        out <- dunif(x = quantile, min = .self$lb, max = .self$ub, log = log)
      }
      return(out)
    },

    rnd = function(n = 1) {
      runif(n, min = .self$lb, max = .self$ub)
    }
  )
)
