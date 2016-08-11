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
    graph = "matrix",
    shiny.app = 'ANY',
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
      .self$graph <- .self$get.graph()
      .self$set.initial()
      .self$set.types()
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
      "Initialize parameters; initialization order is based on graph"

      npar <- nrow(.self$graph)
      pnames <- dimnames(.self$graph)[[1]]
      for (param.name in pnames) {
        .self$set.compute(param.name)
      }
    },

    set.compute = function(param.name, update.timestamp = TRUE) {

      parents <- names(which(.self$graph[param.name,] == 1))
      redundant <- .self$parameters$is.redundant.calculation(
        param.name = param.name, parent.names = parents)
      if (redundant) {

      } else {
        pvec <- mapply(parents, FUN = function(p) {
          paste0(p, " = .self$get.value('", p, "')")
        })
        pstring <- paste0(pvec, collapse = ',')
        fstring <- paste0('.self$get.compute.', param.name, '(', pstring, ')')
        objs <- eval(parse(text = fstring))
        #
        #RTL TODO: replace below with a DISTR test
        #
        if (is.list(objs)) {
          if (names(objs)[1] == 'type') {
            if (objs$type == 'unknown') {
              objs <- runif(1)
            }
          }
        }
        .self$set.value(param.name = param.name, objs = objs,
                        update.timestamp = update.timestamp)
      }
    },

    set.types = function() {
      'Guess the type of parameter from its value
      TODO: use rcvirtual.random for unknown parameters'

      pnames <- dimnames(.self$graph)[[1]]
      for (param.name in pnames) {
        obj <- .self$get.value(param.name)
        #if (is(obj, 'rcvirtual.random')) {
        #  my.type <- 'unknown'
        #} else {
        #  my.type <- 'derived'
        #}
        if (is.list(obj)) {
          if (is.null(obj$type)) {
            my.type <- 'derived'
          } else {
            my.type <- obj$type
          }
        } else {
          my.type <- 'fixed'
        }
        .self$parameters$set.data(param.name = param.name, field.name = "type",
                                  objs = my.type)
      }
    },

    # ------------------------------------------------------
    # Get methods ------------------------------------------
    # ------------------------------------------------------

    get.dependencies = function(x) {
      "Loads the dependencies of parameter x onto the global environment.
      Use this to debug 'get.compute.x' functions"

      args <- names(which(.self$graph[x, ] == 1))
      for (myarg in args) {
        z <- .self$get.value(myarg)
        assign(myarg, z, pos = 1)
      }
      invisible()
    },

    get.time.formatted = function(tbounds.char, tstep = 'day', tz = 'GMT') {
      "Function that formats time information: instants,
      days, months, years, dates, starting instant and
      ending instant."


      #time instants; days since Jan 01, 1970
      time.bounds <- as.POSIXlt(tbounds.char,
                                origin = as.POSIXct('1970/01/01', tz = tz))
      stopifnot(time.bounds[1] <= time.bounds[2])
      if (tstep == "day" | tstep == "days") {
        stp <- 60 ^ 2 * 24
        n <- 1 + as.numeric(
          difftime(time1 = time.bounds[2], time2 = time.bounds[1], tz = tz,
                   units = 'days'))
        inst <- seq(1, n, 1)
        aux <- as.POSIXlt(seq(time.bounds[1], time.bounds[2], by = stp))
        dd <- aux$mday
        mm <- aux$mon + 1
        yy <- 1900 + aux$year
        ci <- mapply(1:n, FUN = function(i) {
          out <- as.numeric(strftime(paste(yy[i], mm[i], dd[i], sep = "-"),
                                     format = "%j"))
        })
      } else if (tstep == "month" | tstep == "months") {
        n <- (time.bounds[2]$year - time.bounds[1]$year) * 12 +
          time.bounds[2]$mon - time.bounds[1]$mon + 1
        inst <- seq(1, n, 1)
        dd <- rep(1, n)
        mm <- (time.bounds[1]$mon + inst - 1) %% 12 + 1
        yy <- 1900 + time.bounds[1]$year +
          floor((time.bounds[1]$mon + inst - 1) / 12)
        ci <- mm
      } else if (tstep == "year" | tstep == "years") {
        n <- time.bounds[2]$year - time.bounds[1]$year + 1
        inst <- seq(1, n, 1)
        dd <- rep(1, n)
        mm <- rep(1, n)
        yy <- 1900 + seq(time.bounds[1]$year, time.bounds[2]$year)
        ci <- rep(1, n)
      } else {
        stop("Timestep not recognized:", tstep)
      }
      dt <- as.POSIXlt(mapply(yy, mm, dd, FUN = function(y, m, d) {
        paste(y, m, d, sep = "/")
      }), tz = tz)
      st <- 1 #first & last instants
      en <- n
      formatted.time <- list(
        inst = inst, cyclical.inst = ci, dd = dd, mm = mm,
        yy = yy, dt = dt, st = st, en = en)
      return(formatted.time)
    },

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
      'Generates a sorted Directed Acyclic Graph of model parameters'

      # Getting list of parameters and their dependencies (aka parents)
      arg.list <- .self$get.args('get.compute.')
      tos <- as.character(mapply(names(arg.list), FUN = function(x) {
        strsplit(x, 'get.compute.')[[1]][2]
      }))
      names(arg.list) <- tos
      froms <- unique(as.character(unlist(arg.list)))
      # Checking that all parents are parameters themselves
      for (fr in froms) {
        if (!(fr %in% tos)) {
          stop(paste0('A function for parameter ', fr, ' has not been defined.'))
        }
      }
      # Auxiliary function
      generate.dependency.matrix <- function(mylist) {
        nargs <- length(mylist)
        mynames <- names(mylist)
        locate <- function(x) which(mynames == x)
        to.pos <- lapply(1:nargs, FUN = function(k) {
          rep(locate(mynames[k]), length(mylist[[k]]))
        })
        from.pos <- lapply(1:nargs, FUN = function(k) {
          mapply(mylist[[k]], FUN = locate)
        })
        pos.mat <- cbind(as.numeric(unlist(to.pos)),
                         as.numeric(unlist(from.pos)))
        m <- matrix(0, nargs, nargs, dimnames = list(to = mynames,
                                                     from = mynames))
        m[pos.mat] <- 1
        return(m)
      }
      # Generating preliminary dependency matrix
      mat <- generate.dependency.matrix(arg.list)
      #
      # Topologically sorting graph, according to Kahn's algorithm
      # https://en.wikipedia.org/wiki/Topological_sorting#Kahn.27s_algorithm
      #
      node.names <- dimnames(mat)[[1]]
      nargs <- nrow(mat)
      i <- 1
      L <- rep(NA, nargs)
      S <- which(rowSums(mat) == 0)
      while (length(S) > 0) {
        n <- S[1]
        S <- if (length(S) > 1) S[2:length(S)] else NULL
        L[i] <- n
        i <- i + 1
        m <- which(mat[, n] == 1)
        len.m <- length(m)
        if (len.m > 0) {
          mat[m, n] <- 0
          p <- if(len.m > 1) (rowSums(mat[m, ]) == 0) else (sum(mat[m, ]) == 0)
          if (sum(p) > 0) S <- c(S, m[p])
        }
      }
      is.dag <- ((i - 1) == nargs)
      stopifnot(is.dag)
      #
      # Constructing a sorted list of parameters and its dependency matrix
      new.list <- lapply(L, FUN = function(j) arg.list[[j]])
      names(new.list) <- node.names[L]
      new.mat <- generate.dependency.matrix(new.list)
      return(new.mat)
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
