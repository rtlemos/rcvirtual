#' Gaussian State variables and vectors
#'
#' This is a Reference Class for Dynamic (Non-)Linear Models
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any random RC must have.
#'
#' @include d-random.R
#' @importFrom rcrandom rcrng
#' @importFrom methods new
#' @export GaussianState
#' @exportClass GaussianState
#'
GaussianState <- setRefClass(
  Class = "GaussianState",
  contains = c("rcvirtual.basic"),
  fields = list(type ='character',
                n.instants = 'numeric',
                nr = 'numeric',
                nc = 'numeric',
                size = 'numeric',
                lb = 'numeric',
                ub = 'numeric',
                m = 'list',
                a = 'list',
                C = 'list',
                R.inv = 'list',
                G = 'matrix',
                constant.var = 'logical'
  ),
  methods = list(
    initialize = function(full.conditional.mean,
                          full.conditional.var,
                          prior.mean,
                          prior.precision,
                          evolution.matrix,
                          n.instants,
                          name = NULL) {
      #
      # Computations for number of instants
      #
      .self$n.instants <- n.instants
      #
      # Computations for the name
      #
      if (is.null(name)) {
        .self$object.name <- as.character(1:n.instants)
      } else if (length(name) == 1) {
        nmb <- formatC(1:n.instants, width = nchar(as.character(n.instants)),
                       format = "d", flag = "0")
        .self$object.name <- paste0(name, nmb)
      } else {
        .self$object.name <- name
      }
      .self$type <- 'GaussianState'
      #
      # Computations for the mean
      #
      get.mean <- function(user.mean) {
        if (is.matrix(user.mean)) {
          mr <- nrow(user.mean)
          mc <- ncol(user.mean)
          if (mr == n.instants & mc == n.instants) {
            stop('Ambiguous format for mean: both dimensions = n.instants')
          }
          if (mr == n.instants) {
            out <- lapply(1:n.instants, FUN = function(i) user.mean[i, ])
          } else if (mr == n.instants) {
            out <- lapply(1:n.instants, FUN = function(i) user.mean[, i])
          }
          stopifnot(mr == n.instants | mc == n.instants)
        } else if (is.numeric(user.mean)) {
          stopifnot(length(user.mean) == n.instants)
          out <- lapply(1:n.instants, FUN = function(i) user.mean[i])
        } else {
          out <- user.mean
        }
        return(out)
      }
      .self$m <- get.mean(full.conditional.mean)
      .self$a <- get.mean(prior.mean)
      .self$nr <- length(.self$m[[1]])
      .self$nc <- 1
      .self$size <- .self$nr
      .self$lb <- -1e10
      .self$ub <- 1e10
      #
      # Computations for the variance
      #
      get.var <- function(user.var) {
        if (.self$nr == 1) {
          if (is.numeric(user.var)) {
            if (length(user.var) == 1) {
              out <- user.var
            } else {
              stopifnot(length(user.var) == n.instants)
              out <- lapply(1:n.instants, FUN = function(i) {
                matrix(var[i], ncol = 1)
              })
            }
          } else if (is.matrix(var)) {
            mr <- nrow(var)
            mc <- ncol(var)
            stopifnot(mr == 1 | mc == 1)
            if (mc == 1 & mr == 1) {
              out <- user.var
            } else if (mr == 1) {
              stopifnot(mc == n.instants)
              out <- lapply(1:n.instants,
                                FUN = function(i) {
                                  matrix(var[1, i], ncol = 1)
                                })
            } else if (mc == 1) {
              stopifnot(mr == n.instants)
              out <- lapply(1:n.instants,
                                FUN = function(i) {
                                  matrix(var[i, 1], ncol = 1)
                                })
            }
          }
        } else if (is.list(user.var)) {
          stopifnot(length(user.var) == n.instants)
          out <- user.var
        }
        return(out)
      }
      .self$C <- get.var(full.conditional.var)
      .self$R.inv <- get.var(prior.precision)
      .self$constant.var <- (length(.self$C) == 1)
      #
      # Computations for the evolution matrix
      #
      .self$G <- evolution.matrix
    },

    rnd = function() {
      if (.self$constant.var) {
        CG <- tcrossprod(.self$C[[tt]], .self$G)
        B <- CG %*% .self$R.inv[[1]]
        H <- .self$C[[1]] - tcrossprod(B, CG)
        U <- chol(H)
        x <- crossprod(U, matrix(nrow = .self$nr, ncol = .self$n.instants - 1,
                                 rnorm(.self$nr * .self$n.instants)))
        out <- .self$m
        out[[.self$n.instants]] <- .self$m[[.self$n.instants]] +
          crossprod(chol(.self$C[[1]]),  rnorm(.self$nr))
        for (tt in seq(.self$n.instants - 1, 1, by = -1)) {
          out[[tt]] <- out[[tt]] + B %*% (out[[tt + 1]] - .self$a[[tt + 1]]) +
            x[, tt]
        }
      } else {
        out <- .self$m
        out[[.self$n.instants]] <- .self$m[[.self$n.instants]] +
          crossprod(chol(.self$C[[.self$n.instants]]),  rnorm(.self$nr))
        x <- lapply(1:(.self$n.instants - 1), FUN = function(tt) {
          CG <- tcrossprod(.self$C[[tt]], .self$G)
          H <- .self$C[[tt]] - CG %*% tcrossprod(.self$R.inv[[tt + 1]], CG)
          res <- crossprod(chol(H), rnorm(.self$nr, mean = 0, sd = 1))
          return(res)
        })
        for (tt in seq(.self$n.instants - 1, 1, by = -1)) {
          B <- tcrossprod(.self$C[[tt]], .self$G) %*% .self$R.inv[[tt + 1]]
          out[[tt]] <- out[[tt]] + B %*% (out[[tt + 1]] - .self$a[[tt + 1]]) +
            x[[tt]]
        }
      }
      return(out)
    }
  )
)
