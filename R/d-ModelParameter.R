#' Model parameter
#'
#' This is a Reference Class for random variables and vectors that enter models
#'
#' @include d-random.R
#' @importFrom rcrandom rcrng
#' @importFrom methods new
#' @export ModelParameter
#' @exportClass ModelParameter
#'
ModelParameter <- setRefClass(
  Class = "ModelParameter",
  contains = c("rcvirtual.random"),
  fields = list(type = 'character',
                prior = 'rcvirtual.random',
                value = 'ANY',
                posterior = 'rcvirtual.random'),
  methods = list(
    initialize = function(prior = NULL, value = NULL, name = NULL) {

      lbound <- if (is.null(prior)) -1e10 else prior$lb
      ubound <- if (is.null(prior)) 1e10 else prior$ub
      callSuper(name = name, type = 'ModelParameter', lb = lbound, ub = ubound)
      #
      # Computations for the prior
      #
      if (!is.null(prior)) {
        .self$prior <- prior
        if (is.null(value)) .self$value <- .self$prior$rnd()
      }
      #
      # Computations for the value
      #
      if (!is.null(value)) {
        .self$value <- value
      } else if (!is.null(prior)) {
        .self$value <- .self$prior$rnd()
      }
      .self$size <- length(.self$value)
    },

    rnd = function(from.posterior = TRUE) {

      out <- if (from.posterior) .self$prior$rnd() else .self$posterior$rnd()
    }

  )
)
