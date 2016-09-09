#' Truncated Normal random variables and vectors
#'
#' This is a Reference Class for truncated normal random variables and vectors
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any random RC must have.
#'
#' @include d-random.R
#' @importFrom rcrandom rcrng
#' @importFrom methods new
#' @export TruncatedNormal
#' @exportClass TruncatedNormal
#'
TruncatedNormal <- setRefClass(
  Class = "TruncatedNormal",
  contains = c("rcvirtual.random"),
  fields = list(mean = 'numeric',
                var = 'matrix',
                ct = 'numeric'),
  methods = list(
    initialize = function(mean, var, lb, ub, ct, name = NULL) {

      callSuper(name = name, type = 'TruncatedNormal', lb = lb, ub = ub)
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
      .self$ct <- 1 #RTL pnorm(ub, mean, sqrt(var)) - pnorm(lb, mean, sqrt(var))
    },

    pdf = function(quantile, log = FALSE) {
      if (quantile < .self$lb | quantile > .self$ub) {
        out <- if (log) -1e10 else 1e-10
      } else {
        dens <- dnorm(x = quantile, mean = .self$mean, sd = sqrt(.self$var),
                     log = log)
        out <- if (log) dens - log(.self$ct) else dens / .self$ct
      }
      return(out)
    },

    rnd = function(n = 1) {
      out <- mapply(1:n, FUN = function(i) {
        bad <- TRUE
        while (bad) {
          res <- rnorm(1, mean = .self$mean, sd = sqrt(.self$var))
          bad <- (res < .self$lb | res > .self$ub)
        }
        return(res)
      })
      return(out)
    }

  )
)
