#' Random variables and vectors
#'
#' This is a virtual Reference Class for random variable and random vector RCs
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any random RC must have.
#'
#' @importFrom rcrandom rcrng
#' @exportClass rcvirtual.random
#'
rcrandom.abstract <- setRefClass(
  Class = "rcvirtual.random",
  contains = c("rcvirtual.basic", "VIRTUAL"),
  fields = list(type = "character",
                univariate = "logical",
                nr = "numeric",
                nc = "numeric",
                param = "list",
                dexpr = "list",
                mode = "list",
                Z = "list",
                rng = "rcrng",
                operations.classes = "list"),
  methods = list(

    size = function(){
      "Provides the size of this object."

      #callSuper(...)
      return(list(nr = .self$nr, nc = .self$nc))
    },

    reset.rng = function(){
      "Resets this object's random number generator"

      .self$rng$reset()
    },

    get.projection  = function(two.vars){
      "Projects variables onto a coordinate system of
      2 anti-variables, if they exist."

      post <- .self$get.marginal(keep.log = TRUE)
      coord <- post$x
      first.c <- coord[1, ]
      last.c <- coord[nrow(coord), ]
      pj <- t(
        matrix(nrow = 4,ncol = .self$np,
               mapply(1:.self$np, FUN = function(j){
                 endp1 <- c(first.c[j],last.c[j])
                 endp2 <- .self$iget.convert(endp1, j)[, two.vars]
                 return(as.numeric(endp2))
               })))
      out <- data.frame(i = factor(1:.self$np), x = pj[, 1],
                        xend = pj[, 2], y = pj[, 3], yend = pj[, 4])
      return(out)
    },

    get.bivariate = function(two.vars){
      "Provides data to plot the posterior
      distribution of variables"

      coord <- expand.grid(
        lapply(1:2, FUN = function(j) {
          post  <- .self$get.marginal(
            id = two.vars[j], keep.log = TRUE)
          my.np <- length(post$x)
          sqnp  <- round(sqrt(my.np))
          endp  <- post$x[c(1,my.np)]
          out <- seq(endp[1], endp[2],
                     length = sqnp)
          return(out)
        }))
      npoints <- nrow(coord)
      myZ <- .self$get.Z()
      zpost <- myZ$get.marginal(keep.log = TRUE)
      zcoord <- zpost$x
      znp <- nrow(zcoord)
      first.zc <- zcoord[1,]
      last.zc <- zcoord[znp,]
      spac <- (last.zc - first.zc) / (znp - 1)
      lb <- myZ$bounds[,1]
      density <- mapply(1:npoints, FUN = function(k){
        idx <- .self$iget.index(
          two.vars = two.vars,
          coords = as.numeric(coord[k, ]),
          lb = lb,
          first.zc = first.zc,
          spac = spac,
          znp = znp)
        dens <- exp(sum(
          mapply(1:.self$np, FUN = function(o){
            zpost$y[idx[o], o]
          })
        ))
        return(dens)
      })
      my.biv  <- data.frame(x = coord[, 1],
                            y = coord[, 2],
                            z = density)
      return(my.biv)
    },

    parameter = function(id, value = NULL, eval = TRUE){
      "Interface to read (value=NULL)
      or write (value not NULL)
      the object's parameters"

      #callSuper(...)
      if (missing(value)) {
        out <- .self$iget.parameter(id = id,
                                    eval = eval)
        return(out)
      } else {
        .self$iset.parameter(id = id, value = value)
      }
    },

    show = function(){
      "Summary of object"

      npr <- length(.self$param)
      ev  <- lapply(1:npr, function(i){
        .self$iget.parameter(id = i, eval = TRUE)
      })
      dv  <- lapply(1:npr, function(i){
        .self$iget.parameter(id = i, eval = FALSE)
      })
      val <- lapply(1:npr, function(i) {
        myev <- ev[[i]]
        if (!is(myev,"RAT")) {
          if (length(myev) == .self$nr) {
            out <- myev
          } else {
            out <- matrix(nrow = .self$nr, myev)
          }
        } else if (is(myev, "Constant")) {
          out <- myev$iget.parameter(id = 1)
        } else {
          out <- dv[[i]]
        }
        return(out)
      })
      pn  <- names(.self$param)
      typ <- if (.self$type == "Constant") {
        ""
      } else if (.self$univariate) {
        "Univariate "
      } else {
        paste0("Multivariate [ r = ",.self$nr," ] ")
      }
      sgv <- unlist(lapply(1:npr, function(i) {
        length(val[[i]]) == 1
      }))
      if (all(sgv)) {
        hd  <- paste0(typ, .self$type, " ( ")
        ft  <- " )\n"
        cre <- paste0(pn[1]," = ",val[[1]])
        if (npr > 1) {
          cre <- c(cre, unlist(
            mapply(2:npr, FUN = function(i){
              paste0(", ", pn[i], " = ",
                     val[[i]])
            })
          ))
        }
        cat(hd, cre, ft, sep = "")
      } else {
        cat(typ, .self$type, "with", pn[1], "\n")
        print(val[[1]])
        if (npr > 1) for (i in 2:npr) {
          cat(pn[i],"\n"); print(val[[i]])
        }
      }
    },

    ###################
    # "Private methods"
    ###################
    is.operation.allowed = function(operation,
                                    argclass){
      allowed <- any(
        .self$operations.classes[[operation]] ==
          argclass)
      return(allowed)
    },

    is.high.resolution = function(){
      return(.self$rng$high.precision())
    },

    iset.parameter = function(id, value){
      "Saves a new value for a given parameter."

      if (.self$type == "Constant") {
        if (is(value, "RAT")) {
          rv <- value$parameter(1, eval = TRUE)
          re <- value$iget.parameter(1,
                                     eval = FALSE)
          sz <- value$size()
        } else {
          rv <- value
          re <- as.character(value)
          if (is(value, "numeric")) {
            sz <- list(nr = length(value), nc = 1)
          } else if (is(value,"matrix")) {
            sz <- list(nr = nrow(value),
                       nc = ncol(value))
          } else {
            stop("iset.parameter: unknown type")
          }
        }
        .self$param[[1]] <- Constant(rv)
        .self$dexpr[[1]] <- re
        .self$nr <- sz$nr
        .self$nc <- sz$nc
      } else {
        if (is(value, "RAT")) {
          rv <- value
          re <- value$iget.parameter(1,
                                     eval = FALSE)
        } else {
          rv <- Constant(value)
          pn <- .self$iget.parameter.name(id)
          re <- paste0(".self$param$", pn)
        }
        .self$param[[id]] <- rv
        .self$dexpr[[id]] <- re
      }
    },

    iset.dexpr = function(id, deparsed.expr){
      "Sets the deparsed expression used to
      construct the object's parameters
      (e.g., mean, variance)"

      .self$dexpr[[id]] <- deparsed.expr
    },

    iset.operate = function(operation, operand,
                            operand.name, operand.side,
                            my.name, ...){
      print("Replace with offspring method")
    },

    iget.parameter = function(id, eval=TRUE, ...){
      "Provides the random variable's parameter value
      (eval=T), or its deparsed expression (eval=F)"

      myexpr <- .self$dexpr[[id]]
      if (eval) {
        if (.self$type == "Constant") {
          out <- mapply(
            seq_along(myexpr),
            FUN = function(i) {
              eval(parse(text = myexpr[i]))
            }
          )
          if (length(out) == 1) out <- out[[1]]
          if (!is(out, "RAT")) {
            if (.self$nc == 1) {
              out <- as.numeric(out)
            } else {
              out <- matrix(nrow = .self$nr,
                            ncold = .self$nc,
                            as.numeric(out))
            }
          }
        } else {
          out <- lapply(
            seq_along(myexpr),
            FUN = function(i){
              eval(parse(text = myexpr[i]))
            }
          )
          if (length(out) == 1) out <- out[[1]]
          if (!is(out,"RAT") & !is(out,"list")) {
            if (length(out) == .self$nr) {
              out <- as.numeric(out)
            } else {
              out <- matrix(nrow = .self$nr,
                            as.numeric(out))
            }
          }
        }
      } else {
        out <- myexpr
      }
      return(out)
    },

    iget.optimization.parameters = function(){
      "This function should be replaced with another
      that does not return NULL, when auxiliary
      parameters exist."
      return(NULL)
    },

    iget.parameter.name = function(id){
      "Provides the name of parameter index id."

      return(names(.self$param)[id])
    },

    iget.Z = function(){
      "Provides the associated z-transformed random
      variable or vector."

      if (is.null(.self[["Z"]])) {
        return(.self)
      } else {
        return(.self$Z)
      }
    },

    iget.convert = function(x, j = NULL){
      "Forward-transformation (i.e., from X to Z) of
      the vector x, or just one element of it."

      if (missing(j)) {
        z <- .self$mode$precR %*%
          (x - .self$mode$par)
      } else {
        precRvec <- .self$mode$precR[,j]
        modej <- .self$mode$par[j]
        z <- t(mapply(x, FUN = function(x) {
          precRvec * (x - modej)
        }))
      }
      return(z)
    },

    iget.zconvert = function(z, j = NULL){
      "Back-transformation (i.e., from Z to X) of
      the vector z, or just one element of it."

      if (missing(j)) {
        x <- .self$mode$covR %*% z + .self$mode$par
      } else {
        covRcol <- .self$mode$covR[,j]
        parj <- .self$mode$par
        x <- t(mapply(z, FUN = function(z) {
          covRcol * z + parj
        }))
      }
      return(x)
    },

    iget.evals = function(){
      zcoord <- lapply(1:.self$nr,FUN = function(j){
        .self$Z[[j]]$iget.coord()
      })
      zlogd <- mapply(1:.self$nr,FUN = function(j){
        .self$Z[[j]]$iget.logd()
      })
      xcoord <- mapply(1:.self$nr,FUN = function(j){
        .self$iget.zconvert(zcoord[[j]])
      })
      return(list(coord = xcoord, logd = zlogd))
    },

    # RTL TODO Not sure if iget.index and iget.zindex
    # are still needed
    iget.index = function(two.vars, coords, lb,
                          first.zc, spac, znp){
      "Provides the indices of the 2 z-variables
      closest to the input coordinates"

      my.vec <- rep(0,.self$np)
      my.vec[two.vars[1]] <-
        coords[1] - .self$specs$par[two.vars[1]]
      my.vec[two.vars[2]] <-
        coords[2] - .self$specs$par[two.vars[2]]
      un <- .self$specs$precR %*% my.vec
      #differs from below ?!
      idx <- round( (un - lb) / spac) + 1
      idx[idx < 1] <- 1
      idx[idx > znp] <- znp
      return(idx)
    },

    iget.zindex = function(two.vars, coords, lb,
                           first.zc, spac, znp){
      "Provides the indices of the 2 z-variables
            closest to the input coordinates"

      my.vec <- rep(0,.self$nr)
      my.vec[two.vars[1]] <- coords[1]
      my.vec[two.vars[2]] <- coords[2]
      un <- my.vec
      idx <- round( (un - lb) / spac) + 1
      idx[idx < 1] <- 1
      idx[idx > znp] <- znp
      return(idx)
    }
  )
)
