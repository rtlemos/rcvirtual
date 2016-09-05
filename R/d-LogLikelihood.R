#' Model log-likelihood
#'
#' This is a Reference Class for the log-likelihood
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any random RC must have.
#'
#' @include v-basic.R
#' @importFrom methods new
#' @export LogLikelihood
#' @exportClass LogLikelihood
#'
LogLikelihood <- setRefClass(
  Class = "LogLikelihood",
  contains = c("rcvirtual.basic"),
  fields = list(type = 'character',
                size = 'numeric',
                lb = 'numeric',
                ub = 'numeric',
                value = 'numeric'),
  methods = list(
    initialize = function(value, name = 'log-likelihood') {

      .self$object.name <- name
      .self$type <- 'LogLikelihood'
      .self$value <- value
      .self$size <- length(.self$value)
      .self$lb <- -1e10
      .self$ub <- 1e10
    }
  )
)
