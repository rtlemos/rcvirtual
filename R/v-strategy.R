#' Statistical model-fitting algorithms (strategy)
#'
#' This is a virtual Reference Class for strategy RCs.
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") to fit statistical models.
#'
#' @field parameters list. Parameters used by strategy.
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
    config = 'data.frame',
    model.res = "list",
    mcmc.res = "matrix",
    model.counter = "numeric",
    model.fitted = "logical",
    use.mle = "logical"),
  methods = list(
    
    # ------------------------------------------------------
    # Initializer methods ----------------------------------
    # ------------------------------------------------------
    
    construct = function() {
      "Constructor of virtual strategy"
      
      callSuper()
      .self$set.graph()
      .self$set.parameters()
      .self$set.config()
      .self$model.counter <- 0
      .self$model.fitted <- FALSE
    },
    
    # ------------------------------------------------------
    # Set methods ------------------------------------------
    # ------------------------------------------------------
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
    
    set.config = function() {
      
      pnames <- .self$get.parameter.names()
      sizes <- unlist(lapply(pnames, FUN = function(param.name) {
        .self$parameters[[param.name]]$size
      }))
      types <- unlist(lapply(pnames, FUN = function(param.name) {
        .self$parameters[[param.name]]$type
      }))
      lb <- unlist(lapply(pnames, FUN = function(param.name) {
        .self$parameters[[param.name]]$lb
      }))
      ub <- unlist(lapply(pnames, FUN = function(param.name) {
        .self$parameters[[param.name]]$ub
      }))
      
      .self$config <- data.frame(
        name = pnames, type = types, size = sizes, lb = lb,
        ub = ub, stringsAsFactors = FALSE
      )
    },
    
    set.optimize = function(mle = TRUE, maxit = 1000){
      "Compute maximum likelihood estimates (MLE) or
      maximum a posteriori estimates (MAP) of unknown model
      parameters"
      
      .self$use.mle <- mle
      # retrieving a vector of unknown parameters
      mp <- .self$config$type == 'ModelParameter'
      unk.parameters <- .self$config$name[mp]
      if (is.null(unk.parameters)) {
        stop("No unknown parameters.")
      }
      derived.parameters <- .self$config$name[.self$config$type == 'Derived']
      ini <- unlist(lapply(unk.parameters, FUN = function(param.name) {
        .self$parameters[[param.name]]$value
      }))
      .self$model.counter <- 0
      .self$model.res <- optim(
        par = ini,
        fn = .self$test.fun,
        gr  = NULL,
        method = "L-BFGS-B",
        lower = .self$config$lb[mp],
        upper = .self$config$ub[mp],
        control = list(fnscale = -1, maxit = maxit),
        hessian = TRUE,
        unk.parameters = unk.parameters,
        unk.size = .self$config$size[mp],
        derived.parameters = derived.parameters)
      names(.self$model.res$par) <- unk.parameters
      dimnames(.self$model.res$hessian) <- list(unk.parameters, unk.parameters)
      .self$model.fitted <- TRUE
    },
    
    set.independence.sampler = function(nits = 10000) {
      npar <- length(.self$model.res$par)
      unk.parameters <- names(.self$model.res$par)
      mp <- .self$config$type == 'ModelParameter'
      derived.parameters <- .self$config$name[.self$config$type == 'Derived']
      variates <- matrix(nrow = npar, ncol = nits, 
                         c(rep(0, npar), rnorm(npar * (nits - 1))),
                         dimnames = list(unk.parameters, NULL))
      R <- chol(-.self$model.res$hessian)
      candidates <- solve(R, variates) + .self$model.res$par
      log.prop.density <- mapply(1:nits, FUN = function(i) {
        sum(variates[, i] ^ 2)
      })
      log.approx.density <- mapply(1:nits, FUN = function(i) {
        .self$test.fun(unk.parameters, candidates[, i], 
                       .self$config$size[mp], derived.parameters)
      })
      log.u <- log(runif(nits))
      x0 <- .self$model.res$par
      lad0 <- log.approx.density[1]
      lpd0 <- log.prop.density[1]
      for (i in 1:nits) {
        log.ratio <- min(0, log.approx.density[i] - lad0 +
                           lpd0 - log.prop.density[i])
        if (log.u[i] <= log.ratio) {
          x0 <- candidates[, i]
          lad0 <- log.approx.density[i]
          lpd0 <- log.prop.density[i]
        } else {
          candidates[, i] <- x0
        }
      }
      .self$mcmc.res <- candidates 
    },
    
    set.value = function(param.name, obj, update.timestamp = TRUE){
      "Shortcut function to store an object in a parameter"
      
      .self$set.data(param.name, field = 'value', obj, update.timestamp)
    },
    
    set.data = function(param.name, field, obj, update.timestamp = TRUE) {
      "Store an object in a parameter"
      
      stopifnot(.self$is.parameter(param.name))
      if (is(obj, 'rcvirtual.basic')) obj <- obj$value
      pstring <- paste0('.self$parameters$', param.name, '$', field, ' <-  obj')
      eval(parse(text = pstring))
    },
    
    set.compute = function(param.name, update.timestamp = TRUE) {
      
      parents <- names(which(.self$graph[param.name,] == 1))
      redundant <- .self$is.redundant.calculation(param.name = param.name,
                                                  parent.names = parents)
      if (redundant) {
        
      } else {
        pvec <- mapply(parents, FUN = function(p) {
          paste0(p, " = .self$get.value('", p, "')")
        })
        pstring <- paste0(pvec, collapse = ',')
        fstring <- paste0('.self$get.compute.', param.name, '(', pstring, ')')
        obj <- eval(parse(text = fstring))
        .self$set.value(param.name = param.name, obj = obj,
                        update.timestamp = TRUE)
      }
    },
    
    # ------------------------------------------------------
    # Get methods ------------------------------------------
    # ------------------------------------------------------
    
    get.log.prior.density = function(unk.parameters) {
      
      lp <- unlist(mapply(unk.parameters, FUN = function(param.name) {
        .self$parameters[[param.name]]$prior$pdf(
          .self$parameters[[param.name]]$value, log = TRUE)
      }))
      #assuming priors are independent
      lpriord <- sum(lp)
      return(lpriord)
    },
    
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
    
    get.posterior.correl = function(method = 'hessian') {
      mat <- switch(
        method,
        'mcmc' = round(cov2cor(var(t(.self$mcmc.res))), 2),
        'hessian' = round(cov2cor(solve(-.self$model.res$hessian)), 2)
      )
      return(mat)
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
      
      out <- .self$config$name[.self$config$type == 'ModelParameter']
      return(out)
    },
    
    get.unknown.parameter.values = function(unlisted = TRUE) {
      
      unk.parameters <- .self$get.unknown.parameters()
      out <- lapply(unk.parameters, FUN = function(param.name) {
        .self$get.value(param.name = param.name)
      })
      names(out) <- unk.parameters
      if(unlisted) out <- unlist(out)
      return(out)
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
    
    test.fun = function(unk.parameters, unk.parvals, unk.size,
                        derived.parameters) {
      
      st <- 0
      for (i in 1:length(unk.parameters)) {
        sz <- unk.size[i]
        val <- unk.parvals[(st + 1):(st + sz)]
        st <- st + sz
        .self$set.value(unk.parameters[i], val)
      }
      for (param.name in derived.parameters) {
        .self$set.compute(param.name = param.name, update.timestamp = TRUE)
      }
      lname <- .self$config$name[.self$config$type == 'LogLikelihood']
      .self$set.compute(param.name = lname, update.timestamp = TRUE)
      if (.self$use.mle) {
        res <- .self$get.log.likelihood()
      } else {
        res <- .self$get.log.prior.density(unk.parameters) +
          .self$get.log.likelihood()
      }
      .self$model.counter <- .self$model.counter + 1
      return(res)
    },
    
    # ------------------------------------------------------
    # Get methods ------------------------------------------
    # ------------------------------------------------------
    
    get.log.likelihood = function(){
      "Returns the log likelihood for a parameter vector"
      
      id <- which(.self$config$type == 'LogLikelihood')
      if (length(id) != 1) stop('One parameter must be of type LogLikelihood')
      out <- .self$parameters[[id]]$value
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
