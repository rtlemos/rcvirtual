#' Inverse Gamma random variables
#'
#' This is a Reference Class for scalar inverse Gamma random variables
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any random RC must have.
#'
#' @include d-random.R
#' @importFrom methods new
#' @export InverseGamma
#' @exportClass InverseGamma
#'
InverseGamma <- setRefClass(
  Class = "InverseGamma",
  contains = c("rcvirtual.random"),
  fields = list(shape = 'numeric',
                rate = 'numeric'
  ),
  methods = list(
    initialize = function(shape, rate, name = NULL) {

      callSuper(name = name, type = 'InverseGamma', lb = 1e-10, ub = 1e10)
      #
      # Computations for parameters rate and shape
      # X ~ IG(rate,shape) => E[X]=shape/(rate-1),
      # Var[X]=shape^2/((rate-1)^2 * (rate-2)), for rate > 2
      # Mode[X] = shape/(rate+1)
      #
      stopifnot(rate > 2)
      .self$rate <- rate
      .self$shape <- shape
    },

    pdf = function(quantile, log = FALSE) {
      if (quantile < .self$lb | quantile > .self$ub) {
        out <- if(log) -1e10 else 1e-10
      } else {
        out <- dgamma(x = 1 / quantile, rate = .self$rate,
                      shape = .self$shape, log = log)
      }
      return(out)
    },

    rnd = function() {
      1 / rgamma(1, rate = .self$rate, shape = .self$shape)
    }
  )
)
