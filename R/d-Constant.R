#' Constant scalars and vectors
#'
#' This is a Reference Class for fixed scalars and vectors
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any random RC must have.
#'
#' @include v-basic.R
#' @importFrom methods new
#' @export Constant
#' @exportClass Constant
#'
Constant <- setRefClass(
  Class = "Constant",
  contains = c("rcvirtual.basic"),
  fields = list(type = 'character',
                size = 'numeric',
                lb = 'numeric',
                ub = 'numeric',
                value = 'ANY'),
  methods = list(
    initialize = function(value = NULL, path = NULL, name = NULL) {

      #
      # Computations for the name and type
      #
      if (is.null(name)) {
        .self$object.name <- 'NA'
      } else {
        .self$object.name <- name
      }
      .self$type <- 'Constant'
      #
      # Computations for the value
      #
      if (!is.null(value)) {
        stopifnot(is.numeric(value) | is.matrix(value))
        .self$value <- value
      } else if (!is.null(path)) {
        ext <- .self$get.extension(path)
        .self$value <- switch(
          ext,
          '.RData' =,
          '.Rdata' =,
          '.rdata' =,
          '.rda' = .self$get.rdata(path),
          stop('Unknown extension.')
        )
      } else {
        stop('Either value or path to file must be provided.')
      }
      #
      # Computation for size
      #
      .self$size <- length(.self$value)
      .self$lb <- .self$ub <- 0
    },

    get.extension = function(path) {
      'Provides the extension of a file'

      x <- regexpr("\\.([[:alnum:]]+)$", path)
      st <- as.numeric(x)
      en <- st + attr(x, "match.length") - 1
      out <- substr(path, start = st, stop = en)
      return(out)
    }

  )
)
