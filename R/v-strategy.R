#' Statistical model-fitting algorithms (strategy)
#'
#' This is a virtual Reference Class for strategy RCs
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any strategy RC must have.
#'
#' @field parameters rcvirtual.parameters. Parameters used by strategy.
#' @field datasets rcvirtual.datasets. Input data sets
#' @field fit.counter numeric. Iteration counter
#' @field model.res list. Model fitting output
#' @field model.fitted logical. Has the model been fitted?
#'
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

      L <- .self$get.value("L")
      out <- if (summarized) sum(unlist(L$llik)) else L$llik
      return(out)
    },

    get.value = function(param.name){
      "Shortcut function to retrieve a parameter value"

      .self$parameters$get.data(param.name = param.name,
                                field.name = "value")
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

    is.skippable = function(force.skip, param.name,
                            parent.names){
      red <- .self$parameters$is.redundant.calculation(
        param.name = param.name,
        parent.names = parent.names)
      out <- force.skip | red
      return(out)
    },

    # ------------------------------------------------------
    # Internal methods -------------------------------------
    # ------------------------------------------------------

    iget.notify = function(par.name){
      if (.self$verbose) cat("computing", par.name, "\n")
    }
  )
)
