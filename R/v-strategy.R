#' Statistical model-fitting algorithms (strategy)
#'
#' This is a virtual Reference Class for strategy RCs
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any strategy RC must have.
#'
#' @field parameters rcvirtual.parameters. Parameters used by strategy.
#' @field fit.counter numeric. Iteration counter
#' @field model.res list. Model fitting output
#' @field model.fitted logical. Has the model been fitted?
#'
#' @importFrom methods new
#' @exportClass rcvirtual.strategy
#'
setRefClass(
  Class = "rcvirtual.strategy",
  contains = c("rcvirtual.basic", "VIRTUAL"),
  fields = list(
    parameters = "rcvirtual.parameters",
    fit.counter = "numeric",
    model.res = "list",
    model.fitted = "logical"),
  methods = list(

    # ------------------------------------------------------
    # Initializer methods ----------------------------------
    # ------------------------------------------------------

    construct = function() {
      "Constructor of virtual strategy"

      callSuper()
      .self$model.fitted <- FALSE
      .self$fit.counter <- 0
    },

    # ------------------------------------------------------
    # Set methods ------------------------------------------
    # ------------------------------------------------------
    set.optimize = function(mle = TRUE){
      "Compute maximum likelihood estimates (MLE) or
      maximum a posteriori estimates (MAP) of unknown model
      parameters"

      #function to optimize
      if (mle) {
        my.fun <- .self$test.log.posterior.density
      } else {
        my.fun <- .self$test.log.likelihood
      }

      # retrieving unknown parameters
      unk.parameters <-
        .self$parameters$get.unk.parameters()

      if (is.null(unk.parameters)) {
        stop("No unknown parameters.")
      }
      ini <- unlist(
        mapply(unk.parameters, FUN = function(dt) dt$value))
      lb <- unlist(
        mapply(unk.parameters, FUN = function(dt) {
          rep(dt$lbound, dt$size)
        }))
      ub <- unlist(
        mapply(unk.parameters, FUN = function(dt) {
          rep(dt$ubound, dt$size)
        }))

      .self$model.res <- optim(
        par = ini,
        fn = my.fun,
        gr  = NULL,
        method = "L-BFGS-B",
        lower = lb,
        upper = ub,
        control = list(fnscale = -1, maxit = 30),
        hessian = TRUE,
        unk.parameters = unk.parameters,
        summarized = TRUE,
        advance.counter = TRUE)
      .self$model.fitted <- TRUE
    },

    set.advance.counter = function(){
      "Advances the strategy's model fitting counter"

      .self$fit.counter <- .self$fit.counter + 1
    },

    set.value = function(param.name, objs,
                         update.timestamp = TRUE){
      "Store an object as a parameter"

      .self$parameters$set.data(
        param.name = param.name, field.name = "value",
        objs = objs, update.timestamp = update.timestamp)
    },

    set.reshape = function(pars, sz){
      for (i in seq_along(pars)) {
        mypar <- pars[i]
        if (.self$parameters$get.size(id = mypar) != sz) {
          if (.self$verbose) {
            cat("reshaping", mypar, "to size", sz, "\n")
          }
          .self$parameters$set.data(
            param.name = mypar, field.name = "size",
            obj = sz, update.timestamp = FALSE)
          .self$parameters$set.data(
            param.name = mypar, field.name = "value",
            update.timestamp = FALSE,
            obj = rep(.self$get.value(mypar), sz))
          # lb <- .self$parameters$get.data(
          #   param.name = mypar, field.name = "lbound")
          # ub <- .self$parameters$get.data(
          #   param.name = mypar, field.name = "ubound")
          # .self$parameters$set.data(
          #   param.name = mypar, field.name = "lbound",
          #   obj = rep(lb, sz))
          # .self$parameters$set.data(
          #   param.name = mypar, field.name = "ubound",
          #   obj = rep(ub, sz))
        }
      }
    },

    set.log.likelihood = function(){
      "Runs the forward filter and sets the likelihood"

      .self$set.adimensional()
      .self$set.spatial()
      .self$set.temporal()
      .self$set.ffbs()
      .self$set.compute.L()

    },

    set.initial = function() {
      "Empty function. Populate with offspring methods"
    },

    # ------------------------------------------------------
    # Get methods ------------------------------------------
    # ------------------------------------------------------

    get.imputation = function(Yall, f, Q, u){
      "Impute missing values from obs & forecasts.
      Yall: vector with some NAs (missing values);
      f: vector of predictive means;
      Q: matrix of predictive covariance;
      u: vector of Gaussian variates;
      output: list with indices of NAs and imputed values"

      summ.na <- sum(is.na(Yall))
      if (summ.na == 0) {
        Y.imputed <- list(idx = NULL, value = NULL)
      } else {
        if (summ.na == length(Yall)) {
          mom <- list(fstar = f, Qstar = Q)
        } else {
          idx.na <- which(is.na(Yall))
          idx.obs <- which(!is.na(Yall))
          Yobs <- Yall[idx.obs]
          fobs <- f[idx.obs]
          fna <- f[idx.na]
          Qobs <- Matrix(Q[idx.obs, idx.obs])
          Qna <- Matrix(Q[idx.na, idx.na])
          Qcov <- Matrix(Q[idx.na, idx.obs])
          Qcond <- Qna -
            Qcov %*% Matrix::solve(Qobs, Matrix::t(Qcov))
          mom <- list(
            fstar = fna +
              Qcov %*% Matrix::solve(Qobs, Yobs - fobs),
            Qstar = Matrix::forceSymmetric(Qcond))
        }
        R <- Matrix::chol(mom$Qstar)
        #u <- rnorm(n = length(mom$fstar))
        Yna <- backsolve(r = R, x = u, upper.tri = TRUE,
                         transpose = TRUE) + mom$fstar
        Y.imputed <- list(idx = idx.na,
                          value = as.numeric(Yna))
      }
      return(Y.imputed)
    },

    # ------------------------------------------------------
    # Test methods -----------------------------------------
    # ------------------------------------------------------
    test.log.posterior.density = function(
      unk.parvals, unk.parameters,
      advance.counter = FALSE, summarized = TRUE){
      "Computes the log-posterior density associated with
      a vector of input parameters"

      #return a single evaluation
      summarized <- TRUE
      #advancing counter here, if required
      if (advance.counter) .self$set.advance.counter()
      #do not advance counter when computing prior and llik
      advc <- FALSE
      lpd <- .self$test.log.prior.density(
        unk.parvals, unk.parameters,
        advc, summarized) +
        .self$test.log.likelihood(
          unk.parvals, unk.parameters,
          advc, summarized)
      return(lpd)
    },

    test.log.prior.density = function(
      unk.parvals, unk.parameters,
      advance.counter = FALSE, summarized = TRUE){
      "Computes the log-prior density for a vector of
      input parameters"

      if (advance.counter) .self$set.advance.counter()
      lp <- unlist(mapply(unk.parameters, FUN = function(u) {
        .self$parameters$get.log.prior.density(
          param.name = u$name, variate = u$value
        )
      }))

      if (summarized) {
        #assuming priors are independent
        lpriord <- sum(lp)
      } else {
        #return a vector of log prior evaluations
        lpriord <- lp
      }
      return(lpriord)
    },

    test.log.likelihood = function(
      unk.parvals, unk.parameters,
      advance.counter = FALSE, summarized = TRUE){
      "Sets the log-likelihood based on
      the provided + current set of parameter values"

      if (advance.counter) .self$set.advance.counter()
      #setting the values of the provided parameters

      if (.self$verbose) print(unk.parvals)

      st <- 1
      for (i in 1:length(unk.parameters)) {
        sz <- unk.parameters[[i]]$size
        unk.parameters[[i]]$value <- unk.parvals[
          st:(st + sz - 1)]
        st <- st + sz
      }

      ok <- mapply(unk.parameters, FUN = function(u) {
        .self$set.value(param.name = u$name,
                        objs = u$value)
        return(TRUE)
      })

      # Computing the log-likelihood
      # NOTE: This function must be implemented by the
      # offspring object
      .self$set.log.likelihood()

      #providing the log-likelihood to the caller
      llik <- .self$get.log.likelihood(
        summarized = summarized)
      return(llik)
    },

    # ------------------------------------------------------
    # Get methods ------------------------------------------
    # ------------------------------------------------------

    get.log.likelihood = function(summarized = TRUE){
      "Returns the log likelihood for a parameter vector"

      L <- .self$parameters$get.data(long.name = 'log-likelihood')
      out <- if (summarized) sum(unlist(L$llik)) else L$llik
      return(out)
    },

    get.value = function(param.name = NULL, long.name = NULL){
      "Shortcut function to retrieve a parameter value"

      .self$parameters$get.value(param.name = param.name, long.name = long.name)
    },

    get.data = function(param.name = NULL,
                        long.name = NULL,
                        field.name = "value") {
      "Shortcut function to retrieve parameter data"

      .self$parameters$get.data(param.name = param.name,
                                long.name = long.name,
                                field.name = field.name)
    },

    get.graph = function() {
      'Generates a Directed (Acyclic) Graph of model parameters'

      arg.list <- .self$get.args('get.compute.')
      tos <- as.character(mapply(names(arg.list), FUN = function(x) {
        strsplit(x, 'get.compute.')[[1]][2]
      }))
      froms <- unique(as.character(unlist(arg.list)))
      unique.args <- unique(c(tos, froms))
      types <- mapply(unique.args, FUN = function(a) {
        .self$parameters$get.data(param.name = a, field.name = 'type')
      })
      nargs <- length(unique.args)
      locate <- function(x) which(unique.args == x)
      to.pos <- lapply(1:length(arg.list), FUN = function(k) {
        rep(locate(tos[k]), length(arg.list[[k]]))
      })
      names(to.pos) <- tos
      from.pos <- lapply(1:length(arg.list), FUN = function(k) {
        mapply(arg.list[[k]], FUN = locate)
      })
      names(from.pos) <- tos
      pos.mat <- cbind(as.numeric(unlist(to.pos)), as.numeric(unlist(from.pos)))
      m <- matrix(0, nargs, nargs, dimnames = list(to = unique.args,
                                                   from = unique.args))
      m[pos.mat] <- 1
      out <- list(names = unique.args,
                  types = types,
                  to.pos = to.pos,
                  from.pos = from.pos,
                  dependency.matrix = m)
    },

    get.ordered.graph = function() {
      "Provides a graph whose nodes have been topologically sorted according to
      Kanh's algorithm.
      Useful to design the update order of a model fitting algorithm.
      https://en.wikipedia.org/wiki/Topological_sorting#Kahn.27s_algorithm"

      gr <- .self$get.graph()
      mat <- gr$dependency.matrix
      nargs <- length(gr$names)
      i <- 1
      L <- rep(NA, nargs)
      S <- which(rowSums(mat) == 0)
      while (length(S) > 0) {
        n <- S[1]
        S <- if (length(S) > 1) S[2:length(S)] else NULL
        L[i] <- n
        i <- i + 1
        m <- which(mat[, n] == 1)
        if (length(m) > 0) {
          mat[m, n] <- 0
          p <- if(length(m) > 1) (rowSums(mat[m, ]) == 0) else (sum(mat[m, ]) == 0)
          if (sum(p) > 0) S <- c(S, m[p])
        }
      }
      is.dag <- ((i - 1) == nargs)
      stopifnot(is.dag)
      R <- mapply(1:nargs, FUN = function(k) which(L == k))
      relocate <- function(x) mapply(x, FUN = function(v) R[v])
      to.pos <- lapply(gr$to.pos, FUN = relocate)
      from.pos <- lapply(gr$from.pos, FUN = relocate)
      pos.mat <- cbind(unlist(to.pos), unlist(from.pos))
      m <- matrix(0, nargs, nargs, dimnames = list(to = gr$names[L],
                                                   from = gr$names[L]))
      m[pos.mat] <- 1
      graph <- list(names = gr$names[L],
                    types = gr$types[L],
                    to.pos = to.pos,
                    from.pos = from.pos,
                    dependency.matrix = m)
      return(graph)
    },

    # ------------------------------------------------------
    # Is methods -------------------------------------------
    # ------------------------------------------------------

    is.valid = function() {
      "Function that checks if strategy is valid."

      if (.self$verbose) cat(
        "rcvirtual.strategy: validating strategy... ")
      # TODO
      cat("OK.\n")
    },

    is.model.fitted = function(){
      "Check if the model has been fitted"

      return(.self$model.fitted)
    },

    is.skippable = function(force.skip, param.name, parent.names){
      if (force.skip) return(TRUE)
      redundant <- .self$parameters$is.redundant.calculation(
        param.name = param.name, parent.names = parent.names)
      return(redundant)
    },

    # ------------------------------------------------------
    # Internal methods -------------------------------------
    # ------------------------------------------------------

    iget.notify = function(par.name){
      if (.self$verbose) cat("computing", par.name, "\n")
    }
  )
)
