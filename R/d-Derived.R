#' Derived scalars and vectors
#'
#' This is a Reference Class for derived model quantities
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any random RC must have.
#'
#' @include v-basic.R
#' @importFrom methods new
#' @export Derived
#' @exportClass Derived
#'
Derived <- setRefClass(
  Class = "Derived",
  contains = c("rcvirtual.basic"),
  fields = list(type = 'character',
                size = 'numeric',
                lb = 'numeric',
                ub = 'numeric',
                value = 'ANY'),
  methods = list(
    initialize = function(value, name = NULL) {

      #
      # Computations for the name and type
      #
      if (is.null(name)) {
        .self$object.name <- 'NA'
      } else {
        .self$object.name <- name
      }
      .self$type <- 'Derived'
      #
      # Computations for the value
      #
      if (is.numeric(value) | is.matrix(value) | is.list(value)) {
        .self$value <- value
      } else {
        stop(name, ' must be either numeric, a matrix, or a list, not a ',
             class(value))
      }
      #
      # Computation for size
      #
      .self$size <- length(.self$value)
      .self$lb <- .self$ub <- 0
    }
  )
)
