#' Constants, random variables and vectors (parameters) of
#' statistical models
#' This is a virtual Reference Class for parameter RCs
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any parameter RC must have
#'
#' @field name character.
#' @field netcdf.io list.
#' @field value list.
#' @field count list.
#' @field mean list.
#' @field variance list.
#' @field diff list.
#'
#' @import ncdf4
#'
#' @exportClass rcvirtual.parameters
#'
setRefClass(
  Class = "rcvirtual.parameters",
  contains = c("rcvirtual.basic", "VIRTUAL"),
  fields = list(
    name = "character",
    netcdf.io = "list",
    value = "list",
    count = "list",
    mean = "list",
    variance = "list",
    diff = "list",
    size = "list",
    timestamp = "list"
  ),
  methods = list(

    # ------------------------------------------------------
    # Initializer methods ----------------------------------
    # ------------------------------------------------------

    construct = function(){
      "Constructor of virtual parameters objects"

      callSuper()

      # Selecting parameters in the model
      crit <- (.self$conf$type != "-")
      df <- as.data.frame(lapply(
        1:length(.self$conf), FUN = function(i) {
          .self$conf[[i]][crit]
        }), stringsAsFactors = FALSE)
      n.p <- nrow(df) # no. parameters
      if (n.p == 0) {
        stop("Model must have at least one parameter.")
      }
      names(df) <- names(.self$conf)

      # Generating lists for objects
      .self$netcdf.io <- .self$value <- .self$mean <- .self$variance <-
        .self$count <- .self$size <- .self$timestamp <-
        vector("list", length = n.p)
      names(.self$netcdf.io) <- names(.self$value) <- names(.self$count) <-
        names(.self$mean) <- names(.self$variance) <- names(.self$size) <-
        names(.self$timestamp) <- df$name

      # Initializing
      for (i in 1:n.p) {
        sz <- .self$size[[i]] <- df$size[i] #no. elements
        zeros <- rep(0, sz)
        .self$value[[i]] <- rep(df$initial[i], sz)
        .self$count[[i]] <- zeros
        .self$mean[[i]] <- zeros
        .self$variance[[i]] <- zeros
        .self$diff[[i]] <- zeros
        # timestamp starts at -1 for objects without
        # initiation (NA), and 0 for initiated objects
        if (!is.na(df$initial[i]) | df$type[[i]] == 'fixed') {
          .self$timestamp[[i]] <- Sys.time()
        } else {
          .self$timestamp[[i]] <- NA
        }
      }

      # Setting the netcdf daemons and slices
      gen.fun <- paste0(.self$package.name, ".netcdf")
      for (i in 1:n.p) {
        fi <- df$input.file[i]
        if (fi != "-") {
          ext <- substr(fi, nchar(fi) - 2, nchar(fi))
          if (ext == ".nc") {
            if (.self$verbose) {
              if (df$long.name[i] != "-") {
                cat("netcdf controller for",
                    df$long.name[i], "(", df$name[i], ")\n")
              } else {
                cat("netcdf controller for",
                    df$name[i], "\n")
              }
            }
            one.inputfile <- (substr(fi, nchar(fi) - 3, nchar(fi) - 3) != '*')
            if (one.inputfile) {
              n <- 1
              .self$netcdf.io[[i]] <- list(
                get(gen.fun)$new(
                  package.name = .self$package.name,
                  object.name = .self$object.name,
                  verbose = FALSE,
                  autoconstruct = FALSE,
                  conf = df[i, ]
                )
              )
            } else {
              root <- substr(fi, 1, nchar(fi) - 4)
              lb <- df$lbound[i]
              ub <- df$ubound[i]
              n <- ub - lb + 1
              .self$netcdf.io[[i]] <- lapply(lb:ub, FUN = function(j) {
                myconf <- df[i, ]
                myconf$input.file <- paste0(root, j, '.nc')
                myconf$lbound <- myconf$ubound <- j
                myncdf <- get(gen.fun)$new(
                  package.name = .self$package.name,
                  object.name = .self$object.name,
                  verbose = FALSE,
                  autoconstruct = FALSE,
                  conf = myconf
                )
                return(myncdf)
              })
            }
            for (j in 1:n) {
              .self$netcdf.io[[i]][[j]]$construct()
            }

            if (.self$conf$store.in.ram[i]) {
              #retrieve the whole data and store it in memory
              bounds <- list(
                lon = .self$get.lon.bounds(),
                lat = .self$get.lat.bounds(),
                time = .self$get.time.bounds(fmt = 'POSIXlt')
              )
              for (j in 1:n) {
                dt <- .self$netcdf.io[[i]][[j]]$get.slice(
                  bounds = bounds)
                .self$set.data(param.name = df$name[i],
                               field.name = "value",
                               objs = dt)
              }
            }
          }
        }
      }

      #setting up time information
      .self$set.time()

      #reading data from files
      .self$set.read.data()
    },

    # ------------------------------------------------------
    # Set methods ------------------------------------------
    # ------------------------------------------------------

    set.time = function(){
      "Function that stores time information into
      rcvirtual.parameters object: \n instants,
      days, months, years, dates, starting instant and
      ending instant."

      #time instants; days since Jan 01, 1970
      w <- .self$get.time.id()
      tstep <- .self$get.time.step()
      tb <- .self$get.time.bounds(fmt = "POSIXlt")
      if (tb[1] > tb[2]) {
        stop("rcvirtual.parameters: bad start/end date")
      } else if (tstep == "day" | tstep == "days") {
        stp <- 60 ^ 2 * 24
        n <- 1 + as.numeric(
          difftime(time1 = tb[2], time2 = tb[1], tz = 'GMT', units = 'days'))
        inst <- seq(1, n, 1)
        aux <- as.POSIXlt(seq(tb[1], tb[2], by = stp))
        dd <- aux$mday
        mm <- aux$mon + 1
        yy <- 1900 + aux$year
        ci <- mapply(1:n, FUN = function(i) {
          out <- as.numeric(strftime(
            paste(yy[i], mm[i], dd[i], sep = "-"),
            format = "%j"))
        })
      } else if (tstep == "month" | tstep == "months") {
        n <- (tb[2]$year - tb[1]$year) * 12 + tb[2]$mon - tb[1]$mon + 1
        inst <- seq(1, n, 1)
        dd <- rep(1, n)
        mm <- (tb[1]$mon + inst - 1) %% 12 + 1
        yy <- 1900 + tb[1]$year + floor((tb[1]$mon + inst - 1) / 12)
        ci <- mm
      } else if (tstep == "year" | tstep == "years") {
        n <- tb[2]$year - tb[1]$year + 1
        inst <- seq(1, n, 1)
        dd <- rep(1, n)
        mm <- rep(1, n)
        yy <- 1900 + seq(tb[1]$year, tb[2]$year)
        ci <- rep(1, n)
      } else {
        stop("Timestep not recognized", tstep)
      }
      dt <- as.POSIXlt(mapply(yy, mm, dd, FUN = function(y, m, d) {
        paste(y, m, d, sep = "/")
      }))
      st <- 1 #first & last instants
      en <- n
      .self$value[[w]] <- list(
        inst = inst, cyclical.inst = ci, dd = dd, mm = mm,
        yy = yy, dt = dt, st = st, en = en)
    },

    set.update.stats = function() {
      for (i in seq_along(.self$conf$name)) {
        mycount <- .self$count[[i]] + 1
        mydiff <- .self$value[[i]] - .self$mean[[i]]
        mymean <- .self$mean[[i]] + mydiff / mycount
        myvar <- .self$variance[[i]] +
          mydiff * (.self$value[[i]] - mymean)
        .self$count[[i]] <- mycount
        .self$diff[[i]] <- mydiff
        .self$mean[[i]] <- mymean
        .self$variance[[i]] <- myvar
      }
    },

    set.data = function(param.name = NULL,
                        field.name = NULL,
                        objs = NULL,
                        update.timestamp = TRUE){
      # param.name = name of parameter (alpha, A, etc)
      # field.name = name of parameter's property
      # (prior mean, lbound, etc)
      # objs = list of objs to be stored

      idx1 <- .self$get.id(param.name = param.name)
      if (!.self$is.valid.parameter(param.name) |
          !.self$is.valid.field(field.name)) {
        stop("Invalid parameter or field name.")
      }
      old <- .self[[field.name]][[param.name]]
      if (identical(old, objs)) {
        return() #no update
      } else {
        .self[[field.name]][[param.name]] <- objs
        if (update.timestamp) {
          .self$timestamp[[param.name]] <- Sys.time()
        }
      }
    },

    set.value = function(param.name = NULL,
                         long.name = NULL,
                         objs) {
      "Shortcut method to set parameter/config values"

      id <- .self$get.id(param.name = param.name,
                         long.name = long.name)
      .self$value[[id]] <- objs
    },

    set.read.data = function(){
      "Reads data from files"

      mytest <- function(x, ext){
        sz.ext <- nchar(ext)
        x.ext <- substr(x, nchar(x) - sz.ext + 1, nchar(x))
        out <- (ext == x.ext)
        return(out)
      }
      crit <- (.self$conf$input.file != "-")
      n.data <- sum(crit)
      if (n.data == 0) return() #nothing to do
      #names of parameters whose data is in data files
      short.names <- .self$conf$name[crit]
      long.names <- .self$conf$long.name[crit]
      id <- seq_len(length(.self$conf$name))[crit]
      file.names <- .self$conf$input.file[crit]
      exts <- c(".RData", ".csv", ".txt")

      for (i in 1:n.data) {
        x <- file.names[i]
        type <- exts[which(mapply(exts, FUN = function(ext){
          mytest(x = x, ext = ext)
        }))]
        if (length(type) > 0) {
          if (.self$verbose) {
            cat("Reading file for", short.names[i],
                "(", long.names[i], ")\n")
          }
          dt <- switch(
            type,
            ".RData" = .self$get.rdata(x),
            ".csv" = .self$get.csv(x),
            ".txt" = .self$get.txt(x)
          )
          .self$set.data(param.name = short.names[i],
                         field.name = "value",
                         objs = dt)
        }
      }
    },

    set.write.data = function(param.name = NA, field.name = NA,
                              id = NA, extension = NA){

      dt <- .self$get.data(param.name = param.name,
                           field.name = field.name,
                           id = id)

      ###########################
      # Writing NetCDF file #####
      ###########################
      if (extension == ".nc") {

        w <- .self$get.time.id()
        tstep <- .self$conf$type[w]
        t.units <- switch(
          tstep,
          seconds, second = "seconds since 1970/01/01",
          minutes, minute = "minutes since 1970/01/01",
          hours, hour = "hours since 1970/01/01",
          days, day = "days since 1970/01/01",
          months, month = "months since 1970/01/01",
          years, year = "years since 1970/01/01",
          stop("set.write.data: unknown time step")
        )
        dimt <- dim.def.ncdf(
          name = "time", units = t.units,
          vals = as.numeric(.self$get.time(frm = "date")))
        diml <- dim(dt[[1]])
        if (is.null(diml)) {
          diml <- length(dt[[1]])
          dimn <- "site"
        } else {
          dimn <- names(diml)
        }
        dimv <- lapply(1:length(dimn), FUN = function(i){
          return(dim.def.ncdf(
            name = paste("dim", i, sep = "_"),
            units = "NA", vals = 1:diml[i]))
        })
        dimv[[length(dimv) + 1]] <- dimt
        varn <- var.def.ncdf(name = param.name,
                             units = "NA", dim = dimv,
                             missval = "NA")
        obj  <- create.ncdf(
          file.path(tempdir(), paste0(param.name, ".nc")),
          varn)
        cnt  <- as.integer(c(diml, 1))
        for (i in 1:length(dt)) {
          put.var.ncdf(obj, varn , dt[[i]],
                       start = c(rep(1, length(diml)), i),
                       count = cnt)
        }
        ok <- sync.ncdf(obj) #saving to disk
        ok <- close.ncdf(obj) #closing the file
      }
    },

    set.lon.bounds = function(lon.bounds){
      "Sets the min/max latitude of data used in model"

      lon.idx <- .self$get.id(long.name = "longitude")
      .self$conf$lbound[lon.idx] <- lon.bounds[1]
      .self$conf$ubound[lon.idx] <- lon.bounds[2]
    },

    set.lat.bounds = function(lat.bounds){
      "Sets the min/max latitude of data used in model"

      lat.idx <- .self$get.id(long.name = "latitude")
      .self$conf$lbound[lat.idx] <- lat.bounds[1]
      .self$conf$ubound[lat.idx] <- lat.bounds[2]
    },

    set.initial = function() {
      "Empty function. Populate with offspring methods"
    },

    # ------------------------------------------------------
    # Get methods ------------------------------------------
    # ------------------------------------------------------
    get.time = function(idx = NULL, frm = "instants"){
      "Retrieves time information from object"

      w <- .self$get.time.id()
      #testing idx
      if (is.null(idx)) {
        i <- seq(along = .self$value[[w]]$inst )
      } else if (idx >= .self$value[[w]]$st &
                 idx <= .self$value[[w]]$en) {
        i <- idx - .self$value[[w]]$st + 1
      } else {
        warning("get.time: out of bounds")
        return()
      }
      #testing frm
      out <- switch(
        frm,
        instants = .self$value[[w]]$inst[i],
        cyclical.instants =
          .self$value[[w]]$cyclical.inst[i],
        days = .self$value[[w]]$dd[i],
        months = .self$value[[w]]$mm[i],
        years = .self$value[[w]]$yy[i],
        dates = .self$value[[w]]$dt[i],
        bounds = c(.self$value[[w]]$st,
                   .self$value[[w]]$en),
        warning("get.time: unimplemented")
      )
      return(out)
    },

    get.time.id = function(){
      aux.f <- function(x) return(x$long.name)

      w <- .self$get.id(long.name = "time")
      if (length(w) != 1) {
        stop("rcvirtual.parameters get.time.id:
             problem with time parameter")
      }
      return(w)
      },

    get.time.step = function(frm = "character"){
      "Provides the time step of the model, either as a
      string ('year', 'month', 'day'), or as a
      number [days] (1, 31, 365)."

      w <- .self$get.time.id()
      tstep.char <- .self$conf$units[[w]]
      if (frm == "character") {
        out <- tstep.char
      } else if (frm == "numeric") {
        out <- switch(tstep.char,
                      day = 1,
                      month = 31,
                      year = 365)
      }
      return(out)
    },

    get.time.bounds = function(fmt = "POSIXlt"){
      "Lower and upper time bounds
      (days since Jan 01, 1970)"

      w <- .self$get.time.id()
      ini <- c(.self$conf$lbound[[w]],
               .self$conf$ubound[[w]])
      out <- switch(
        fmt,
        numeric = ini,
        POSIXlt = as.POSIXlt(ini, origin = as.POSIXct('1970/01/01')),
        list = list(as.POSIXlt(ini[1], origin = as.POSIXct('1970/01/01')),
                    as.POSIXlt(ini[2], origin = as.POSIXct('1970/01/01')))
      )
      return(out)
    },

    get.lon.bounds = function(){
      "Provides the min/max longitude of data used in model"

      lon.idx <- .self$get.id(long.name = "longitude")
      lon.bounds <- c(.self$conf$lbound[lon.idx],
                      .self$conf$ubound[lon.idx])
      return(lon.bounds)
    },

    get.lat.bounds = function(){
      "Provides the min/max latitude of data used in model"

      lat.idx <- .self$get.id(long.name = "latitude")
      lat.bounds <- c(.self$conf$lbound[lat.idx],
                      .self$conf$ubound[lat.idx])
      return(lat.bounds)
    },

    get.size = function(id = NULL, param.name = NULL,
                        long.name = NULL){
      "Provides the size of an object, identified by one of
      the following: id, name or long.name"

      if (is.null(id)) {
        id <- .self$get.id(param.name = param.name,
                           long.name = long.name)
      }
      return(.self$size[[id]])
    },

    get.names = function(id = NULL, long = FALSE){
      "Provides the name of the object with index id"

      if (is.null(id)) {
        if (long) {
          out <- .self$conf$long.name
        } else {
          out <- .self$conf$name
        }
      } else {
        if (long) {
          out <- .self$conf$long.name[id]
        } else {
          out <- .self$conf$name[id]
        }
      }
      return(out)
    },

    get.id = function(param.name = NULL, long.name = NULL){
      "Given a name, provides the parameter ID.
      Works for vectors of names"

      if (!is.null(param.name)) {
        id <- mapply(param.name, FUN = function(nm){
          # first object whose name is nm
          # (the [1] is a safety measure against NAs)
          which(.self$conf$name == nm)[1]
        })
      } else if (!is.null(long.name)) {
        id <- mapply(long.name, FUN = function(lnm){
          which(.self$conf$long.name == lnm)[1]
        })
      } else {
        id <- NULL
      }
      return(id)
    },

    get.data = function(param.name = NULL,
                        long.name = NULL,
                        field.name = "value"){
      "param.name = name of parameter (alpha, A, etc)
      field.name = name of parameter's property
      (prior mean, lbound, value, etc.)"

      id <- .self$get.id(param.name = param.name,
                         long.name = long.name)
      if (.self$is.valid.field(field.name)) {
        out <- .self[[field.name]][[id]]
      } else if (any(names(.self$conf) == field.name)) {
        out <- .self$conf[[field.name]][[id]]
      } else if (!is.null(.self$netcdf.io[[id]][[1]])) {
        fnames <- names(.self$netcdf.io[[id]][[1]]$fields())
        if (any(fnames == field.name)) {
          out <- .self$netcdf.io[[id]][[1]][[field.name]]
        } else {
          out <- NULL
        }
      } else {
        out <- NULL
      }
      return(out)
    },

    get.slice = function(param.name = NULL,
                         long.name = NULL,
                         bounds = NULL) {
      id <- .self$get.id(param.name = param.name,
                         long.name = long.name)
      if (.self$store.in.ram) {
        stop('Not yet implemented')
      } else {
        for (j in 1:length(.self$netcdf.io[[id]])) {
          out <- .self$netcdf.io[[id]][[j]]$get.slice(bounds = bounds)
          if (!is.null(out)) break
        }
      }
      return(out)
    },

    get.value = function(param.name = NULL,
                         long.name = NULL){
      "Rapid access to parameter value"

      if (!is.null(param.name)) {
        dt <- .self$value[[param.name]]
      } else {
        dt <- .self$get.data(long.name = long.name,
                             field.name = "value")
      }
      return(dt)
    },

    get.unk.parameters = function(){
      "Provides a list of names of parameters whose
      MLE/MAP/posterior will be or has been computed,
      as well as their current values and
      upper/lower bounds"

      pnames <- .self$get.names()
      ptypes <- unlist(
        lapply(pnames, FUN = .self$get.data,
               field.name = "type"))
      unk.parameters.char <- pnames[ptypes == "unknown"]
      if (length(unk.parameters.char) == 0) {
        return(NULL)
      }

      specs <- c("lbound", "ubound", "size", "value")
      unk <- lapply(
        unk.parameters.char, FUN = function(p){
          out <- lapply(specs, FUN = function(s) {
            .self$get.data(param.name = p,
                           field.name = s)
          })
          out$name <- p
          names(out) <- c(specs, "name")
          return(out)
        })
      names(unk) <- unk.parameters.char
      return(unk)
    },

    get.log.prior.density = function(param.name, variate){
      "Computes the log-prior density"

      id.int <- .self$get.id(param.name = param.name)
      if (length(id.int) != 1) stop("problem with id.int")
      pdistr <- .self$conf$prior.distr[[id.int]]
      pmean <- .self$conf$prior.mean[[id.int]]
      pvar <- .self$conf$prior.var[[id.int]]
      plb <- .self$conf$lbound[[id.int]]
      pub <- .self$conf$ubound[[id.int]]
      out <- .self$get.density(
        variate = variate, distr = pdistr, mean = pmean,
        var = pvar, lb = plb, ub = pub, dolog = TRUE)
      return(out)
    },

    get.log.proposal.density = function(param.name,
                                        variate){
      "Computes the log-proposal density"

      id.int <- .self$get.id(param.name = param.name)
      if (length(id.int) != 1) return(NA)
      pdistr <- .self$proposal.distr[[id.int]]
      pmean <- .self$conf$value[[id.int]]
      pvar <- .self$conf$proposal.var[[id.int]]
      plb <- .self$conf$lbound[[id.int]]
      pub <- .self$conf$ubound[[id.int]]
      out <- .self$get.density(
        variate = variate, distr = pdistr, mean = pmean,
        var = pvar, lb = plb, ub = pub, dolog = TRUE)
      return(out)
    },

    get.density = function(variate, distr, mean = mean, var,
                           lb, ub, dolog){
      "Computes a p.d.f., based on input specifications"

      if (is.null(distr)) stop("distr cannot be NULL")
      d <- switch(distr,
                  # Normal
                  "No" = dnorm(x = variate, mean = mean,
                               sd = sqrt(var), log = dolog),
                  # Gamma, E(X) = a*s, Var(X) = a*s^2,
                  # a=shape, s=scale
                  "Ga" = dgamma(x = variate,
                                shape = mean ^ 2 / var,
                                scale = var / mean,
                                log = dolog),
                  # Uniform
                  "-" = ,
                  "Un" = dunif(x = variate, min = lb,
                               max = ub, log = dolog),
                  # Beta,  mean=a/(a+b),
                  # var=ab/((a+b)^2 (a+b+1))
                  "Be" = if (lb == 0 & ub == 1) {
                    b <- mean / var * (1 - mean) ^ 2 +
                      mean - 1
                    a <- b * mean / (1 - mean)
                    dd <- dbeta(x = variate, shape1 = a,
                                shape2 = b, log = dolog)
                    return(dd)
                  } else {
                    stop(paste0(
                      "rcvirtual.parameters get.density:",
                      "non-standard Beta not implemented"))
                  },
                  stop("Unknown distribution type")
      )
      return(d)
    },

    # ------------------------------------------------------
    # Is methods -------------------------------------------
    # ------------------------------------------------------

    is.valid = function(obj){
      "Function that checks if parameters are valid."

      if (.self$verbose) print(
        "rcvirtual.parameters: validating parameters...")
      fatal.errors <- FALSE

      #checking if Y exists and is a list
      Y <- .self$get.data(param.name = "Y",
                          field.name = "value")
      if (all(is.na(Y))) {
        warning(paste0("rcvirtual.parameters validate: ",
                       "missing response Y."))
        fatal.errors <- TRUE
      } else if (class(Y) == "data.frame") {
        if (.self$verbose) print(paste0(
          "rcvirtual.parameters validate: ",
          "converting Y to a list"))
        #number of observation per time instant
        nobs <- dim(.self$get.data(param.name = "l",
                                   field.name = "value"))[1]
        if (dim(Y)[1] == nobs) {
          # converting a matrix to a list, column by column
          new.y <- lapply(seq_len(ncol(Y)),
                          FUN = function(i) Y[,i])
        } else if (dim(Y)[2] == nobs) {
          # converting a matrix to a list, row by row
          new.y <- lapply(seq_len(nrow(Y)),
                          FUN = function(i) Y[i,])
        } else {
          msg <- "rcvirtual.parameters validate:
          don't know how to convert Y to a list."
          if (.self$verbose) print(msg)
          new.y <- Y
        }
        .self$set.data(param.name = "Y",
                       field.name = "value",
                       objs = new.y,
                       update.timestamp = FALSE)
      }

      #checking if number of time steps matches the number
      # of instances with observations
      tt <- .self$get.time(frm = "POSIXlt")
      Y  <- .self$get.data(param.name = "Y",
                           field.name = "value")
      if (length(tt) != length(Y)) {
        tstep <- .self$get.time.step("character")
        if (tstep == "day") {
          aux <- as.POSIXlt(tt[1] + length(Y) - 1)
          dd <- aux$mday
          mm <- aux$mon + 1
          yy <- aux$year + 1900
        } else if (tstep == "month") {
          lb <- tt[1] #day, month, year
          dd <- 1
          mm <- (lb$mon + length(Y) - 1) %% 12 + 1
          yy <- 1900 + lb$year +
            floor((lb$month + length(Y) - 1) / 12)
        } else if (tstep == "year") {
          lb <- tt[1]
          ub <- tt[length(tt)]
          n <- ub$year - lb$year + 1
          dd <- rep(1, n)
          mm <- rep(1, n)
          yy <- 1900 + lb$year + length(Y) - 1
        }
        msg <- paste0(
          "rcvirtual.parameters validate:
          number of time steps (", length(tt),
          ") does not match the number of instances
          with observations (", length(Y), ").\n",
          "Suggested fix: consult
          .self$conf$parameters$long.name and
          .self$conf$parameters$ubound \n",
          "and change upper bound of time to ",
          mm, "/", dd, "/", yy)
        warning(msg)
        fatal.errors <- TRUE #this is a fatal error
      }

      #checking if the no. observation sites is constant
      Y <- .self$get.data(param.name = "Y",
                          field.name = "value")
      ltime  <- mapply(Y, FUN = function(x) length(x))
      N <- as.numeric(.self$get.data(param.name = "N",
                                     field.name = "value"))
      if (!is.na(N)) if (any(ltime !=  N)) {
        msg <- paste(
          "rcvirtual.parameters validate:",
          "no. observation locations changes over time.\n",
          "Suggested fix: pad observations with NAs.")
        warning(msg)
      }

      # checking if the no. observation sites matches
      # no. coordinates provided
      l <- .self$get.data(param.name = "l",
                          field.name = "value")
      N <- as.numeric(.self$get.data(
        param.name = "N", field.name = "value"))
      if (!is.null(N) & !is.null(l)) if (N != nrow(l)) {
        msg <- paste(
          "rcvirtual.parameters validate:",
          "no. observation locations differs from N.\n",
          "Suggested fix:",
          "check coordinates of locations against
          observations data file.")
        warning(msg)
      }

      if (!fatal.errors & .self$verbose) {
        print("parameters ok")
      }
    },

    is.valid.parameter = function(param.name){
      "Tests whether fname is among the parameter names of
      this object"

      if (is.null(param.name)) {
        test <- FALSE
      } else {
        test <- any(param.name == .self$conf$name)
      }
      return(test)
    },

    is.valid.field = function(field.name){
      "Tests whether fname is among the field names of
      this object"

      if (is.null(field.name)) {
        test <- FALSE
      } else {
        test <- any(field.name == names(.self$fields()))
      }
      return(test)
    },

    is.redundant.calculation = function(param.name = NA, parent.names = NULL){
      "Should a derived quantity be recomputed?"

      my.timestamp <- .self$get.data(param.name = param.name,
                                     field.name = 'timestamp')
      if (is.na(my.timestamp)) return(FALSE) #don't skip: not computed yet
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
    }
  )
)
