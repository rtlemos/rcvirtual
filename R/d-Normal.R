#' Normal random variables and vectors
#'
#' This is a Reference Class for normal random variables and vectors
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any random RC must have.
#'
#' @include d-random.R
#' @importFrom rcrandom rcrng
#' @importFrom methods new
#' @export Normal
#' @exportClass Normal
#'
Normal <- setRefClass(
  Class = "Normal",
  contains = c("rcvirtual.random"),
  fields = list(mean = 'numeric',
                var = 'matrix'),
  methods = list(
    initialize = function(mean, var, name = NULL) {

      callSuper(name = name, type = 'Normal',
                lb = -.Machine$double.xmax, ub = .Machine$double.xmax)
      #
      # Computations for the mean
      #
      if (is.matrix(mean)) {
        stopifnot(ncol(mean) == 1)
        .self$mean <- as.numeric(mean)
      } else {
        .self$mean <- mean
      }
      .self$size <- length(.self$mean)
      #
      # Computations for the variance
      #
      if (.self$size == 1) {
        if (is.numeric(var)) {
          .self$var <- matrix(var, nrow = .self$size)
        }
      } else {
        .self$var <- var
      }
      stopifnot(nrow(.self$var) == .self$size & ncol(var) == .self$size)
    },

    pdf = function(quantile, log = FALSE) {
      dnorm(x = quantile, mean = .self$mean, sd = sqrt(.self$var), log = log)
    },

    rnd = function() {
      rnorm(1, mean = .self$mean, sd = sqrt(.self$var))
    }

  )
)
