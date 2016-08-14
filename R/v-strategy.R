#' Statistical model-fitting algorithms (strategy)
#'
#' This is a virtual Reference Class for strategy RCs.
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") to fit statistical models.
#'
#' @field parameters list. Parameters used by strategy.
#' @field fit.counter numeric. Iteration counter
#' @field model.res list. Model fitting output
#' @field model.fitted logical. Has the model been fitted?
#'
#' @include v-basic.R
#' @importFrom methods new
#' @exportClass rcvirtual.strategy
#'
setRefClass(
  Class = "rcvirtual.strategy",
  contains = c("rcvirtual.basic", "VIRTUAL"),
  fields = list(
    parameters = "list",
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
      .self$set.graph()
      .self$set.parameters()
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

      # retrieving a vector of unknown parameters
      unk.parameters <- .self$get.unknown.parameters()
      unk.size <- mapply(unk.parameters, FUN = function(param.name) {
        .self$parameters[[param.name]]$size
      })

      if (is.null(unk.parameters)) {
        stop("No unknown parameters.")
      }
      ini <- unlist(mapply(unk.parameters, FUN = function(param.name) {
        .self$parameters[[param.name]]$value
      }))
      lb <- unlist(
        mapply(unk.parameters, unk.sz, FUN = function(param.name, sz) {
          mylb <- .self$parameters[[param.name]]$lb
          if (length(mylb) != sz) mylb <- rep(mylb[1], sz)
          return(mylb)
        }))
      ub <- unlist(
        mapply(unk.parameters, unk.sz, FUN = function(param.name, sz) {
          myub <- .self$parameters[[param.name]]$ub
          if (length(myub) != sz) myub <- rep(myub[1], sz)
          return(myub)
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
        unk.size = unk.size)
      .self$model.fitted <- TRUE
    },

    set.advance.counter = function(){
      "Advances the strategy's model fitting counter"

      .self$fit.counter <- .self$fit.counter + 1
    },

    set.value = function(param.name, obj, update.timestamp = TRUE){
      "Shortcut function to store an object in a parameter"

      .self$set.data(param.name, field = 'value', obj, update.timestamp)
    },

    set.data = function(param.name, field, obj, update.timestamp = TRUE) {
      "Store an object in a parameter"

      stopifnot(.self$is.parameter(param.name))
      pstring <- paste0('.self$parameters$', param.name, '$', field.name,
                        ' <- ', obj)
      eval(parse(text = pstring))
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

    set.parameters = function() {
      "Constructs a list of parameters required to fit a model"

      #
      # Creating the list of parameters
      #
      pnames <- .self$get.parameter.names()
      npar <- length(pnames)
      .self$parameters <- vector('list', length = npar)
      names(.self$parameters) <- pnames
      #
      # Populating the list, in the correct order (provided by the graph)
      #
      for(param.name in pnames) {
        #
        # Parents of this parameter
        #
        parents <- names(which(.self$graph[param.name,] == 1))
        #
        # Calling the user's "get.compute
        pvec <- mapply(parents, FUN = function(p) {
          paste0(p, " = .self$get.value('", p, "')")
        })
        pstring <- paste0(pvec, collapse = ',')
        fstring <- paste0('.self$get.compute.', param.name, '(', pstring, ')')
        res <- eval(parse(text = fstring))
        if (is(res, 'rcvirtual.basic')) {
          if (is(res, 'rcvirtual.random')) {
            #model parameters
            obj <- ModelParameter(prior = res,
                                  value = res$rnd(),
                                  name = res$object.name)
          } else {
            #constants
            obj <- res
          }
        } else {
          #derived objects
          obj <- Derived(name = param.name, value = res)
        }
        .self$parameters[[param.name]] <- obj
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

    get.parameter.names = function() {
      'Retrieve the names of parameters in the model,
      ordered according to the graph'

      out <- dimnames(.self$graph)[[1]]
      return(out)
    },

    get.unknown.parameters = function() {
      'Retrieve the names of rcbasic.random parameters'


    },

    get.bounds = function(param.name = 'latitude') {
      'Provides the bounds for a given parameter'

      x <- .self$get.value(param.name)
      bounds <- c(min(x), max(x))
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
    test.log.posterior.density = function(unk.parvals, unk.parameters, unk.size) {
      "Computes the log-posterior density associated with
      a vector of input parameters"

      lpd <- .self$test.log.prior.density(unk.parvals, unk.parameters, unk.size) +
        .self$test.log.likelihood(unk.parvals, unk.parameters, unk.size)
      return(lpd)
    },

    test.log.prior.density = function(unk.parvals, unk.parameters, unk.size){
      "Computes the log-prior density for a vector of
      input parameters"

      lp <- unlist(mapply(unk.parameters, FUN = function(u) {
        .self$parameters$get.log.prior.density(
          param.name = u$name, variate = u$value
        )
      }))
      #assuming priors are independent
      lpriord <- sum(lp)
      return(lpriord)
    },

    test.log.likelihood = function(
      unk.parvals, unk.parameters,
      advance.counter = FALSE){
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

    set.graph = function() {
      'Generates a sorted Directed Acyclic Graph of model parameters'

      #
      # Getting list of parameters and their dependencies (aka parents)
      #
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
      #
      # Auxiliary function
      #
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
      #
      # Generating preliminary dependency matrix
      #
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
      #
      new.list <- lapply(L, FUN = function(j) arg.list[[j]])
      names(new.list) <- node.names[L]
      new.mat <- generate.dependency.matrix(new.list)
      #
      # storing
      #
      .self$graph <- new.mat
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

    get.value = function(param.name){
      "Shortcut function to retrieve a parameter value"

      .self$get.data(param.name, field = 'value')
    },

    get.data = function(param.name, field.name = "value") {
      "Retrieve parameter data"

      stopifnot(.self$is.parameter(param.name))
      pstring <- paste0('.self$parameters$', param.name, '$', field.name)
      out <- eval(parse(text = pstring))
      return(out)
    },

    get.parameter.types = function() {
      'Retrieve the types (classes) of all model parameters, ordered according
      to the graph'

      pnames <- .self$get.parameter.names()
      out <- mapply(pnames, FUN = function(param.name) {
        .self$parameters[[param.name]]$type
      })
      return(out)
    },

    # ------------------------------------------------------
    # Is methods -------------------------------------------
    # ------------------------------------------------------

    is.parameter = function(param.name) {
      "Check if param.name is among the list of model parameters"

      out <- param.name %in% .self$get.parameter.names()
      return(out)
    },

    is.model.fitted = function(){
      "Check if the model has been fitted"

      return(.self$model.fitted)
    },

    is.redundant.calculation = function(param.name = NA, parent.names = NULL){
      "Should a derived quantity be recomputed?"

      my.timestamp <- .self$get.data(param.name = param.name,
                                     field.name = 'timestamp')
      if (length(my.timestamp) == 0) return(FALSE) #don't skip: not computed yet
      if (is.na(my.timestamp)) return(FALSE) # ""
      if (length(parent.names) == 0) return(TRUE) #skip: no dependencies
      parent.timestamps <- mapply(parent.names, FUN = function(nm) {
        .self$get.data(param.name = nm, field.name = 'timestamp')
      })
      if (any(is.na(parent.timestamps))) {
        cat('Problem computing ', param.name, '\n')
        cat('NA timestamp(s); parent(s) should be computed first. \n')
        cat('Parents: ', parent.names, '\n')
        cat('Timestamps: ', parent.timestamps, '\n')
        return(TRUE)
      }
      if (any(parent.timestamps > my.timestamp)) {
        return(FALSE) #don't skip: updated parent
      } else {
        return(TRUE) #skip: more recent than parent(s)
      }
    },

    # ------------------------------------------------------
    # Internal methods -------------------------------------
    # ------------------------------------------------------

    iget.notify = function(par.name){
      if (.self$verbose) cat("computing", par.name, "\n")
    }
  )
)
