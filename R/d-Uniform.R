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
    initialize = function(lb, ub, name = NULL) {

      #
      # Computations for the name
      #
      if (is.null(name)) {
        .self$object.name <- 'NA'
      } else {
        .self$object.name <- name
      }
      .self$type <- 'Uniform'
      #
      # Computations for bounds
      #
      .self$lb <- lb
      .self$ub <- ub
    },

    rnd = function() {
      runif(1, min = .self$lb, max = .self$ub)
    }
  )
)
