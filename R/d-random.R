#' Random variables and vectors
#'
#' This is a virtual Reference Class for random variable and random vector RCs
#'
#' @include v-basic.R
#' @importFrom rcrandom rcrng
#' @importFrom methods new
#' @exportClass rcvirtual.random
#'
setRefClass(
  Class = "rcvirtual.random",
  contains = c("rcvirtual.basic", "VIRTUAL"),
  fields = list(type = 'character',
                univariate = "logical",
                nr = "numeric",
                nc = "numeric",
                rng = "rcrng"),
  methods = list(

    size = function(){
      "Provides the size of this object."

      #callSuper(...)
      return(list(nr = .self$nr, nc = .self$nc))
    },

    reset.rng = function(){
      "Resets this object's random number generator"

      .self$rng$reset()
    }
  )
)
