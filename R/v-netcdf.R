#' rcvirtual.netcdf: virtual RC that handles netcdf files
#'
#' @field location character.
#' @field mydata ANY.
#' @field is.spatial.avg logical.
#' @field is.time.indexed logical.
#' @field dim.names character.
#' @field var.names character.
#' @field var.units character.
#' @field var.descr character.
#' @field lon.name character.
#' @field lat.name character.
#' @field time.name character.
#' @field lon.bounds numeric.
#' @field lat.bounds numeric.
#' @field spatial.resolution numeric.
#' @field time.bounds POSIXlt.
#' @field lon360 logical.
#' @field start.date POSIXlt.
#' @field time.units character.
#' @field time.mult numeric.
#' @field n.spatial numeric.
#' @field instants POSIXlt.
#'
#' @import ncdf4
#' @exportClass rcvirtual.netcdf
#'
setRefClass(
  Class = "rcvirtual.netcdf",
  contains = "rcvirtual.basic",
  fields = list(location = "character",
                mydata = "ANY",
                is.spatial.avg = "logical",
                is.time.indexed = "logical",
                dim.names = "character",
                var.names = "character",
                var.units = "character",
                var.descr = "character",
                lon.name = "character",
                lat.name = "character",
                time.name = "character",
                lon.bounds = "numeric",
                lat.bounds = "numeric",
                spatial.resolution = "numeric",
                time.bounds = "POSIXlt",
                lon360 = "logical",
                start.date = "POSIXlt",
                time.units = "character",
                time.mult = "numeric",
                n.spatial = "numeric",
                instants = "POSIXlt"
  ),
  methods = list(

    # ------------------------------------------------------
    # Initializer methods ----------------------------------
    # ------------------------------------------------------

    construct = function(){
      "Construction of myNetCDF objects."

      callSuper()

      .self$is.spatial.avg <- .self$conf$is.spatial.avg
      netcdf.opened <- FALSE
      loc <- .self$conf$input.file
      if (is.character(loc)) {
        prefix <- substr(loc, 1, 2)
        if (prefix == "tcc") {
          .self$mydata <- jules.get.entity(loc)
        } else {
          .self$mydata <- ncdf4::nc_open(loc)
          netcdf.opened <- TRUE
        }
        .self$location <- loc
      } else {
        .self$location <- "local.file"
        .self$mydata <- loc
      }
      .self$dim.names <- names(.self$mydata$dim)
      .self$var.names <- names(.self$mydata$var)
      .self$var.units <- mapply(
        .self$var.names, FUN = function(myname){
          ncdf4::ncatt_get(
            .self$mydata, myname, "units")$value
        })
      if (any(.self$dim.names == "latitude") |
          any(.self$var.names == "latitude")) {
        .self$lat.name <- "latitude"
      } else {
        .self$lat.name <- "lat"
      }
      if (any(.self$dim.names == "longitude") |
          any(.self$var.names == "longitude")) {
        .self$lon.name <- "longitude"
      } else {
        .self$lon.name <- "lon"
      }
      if (any(.self$dim.names == "time") |
          any(.self$var.names == "time")) {
        .self$time.name <- "time"
      } else {
        .self$time.name <- "NA"
      }
      lon  <- ncdf4::ncvar_get(
        .self$mydata, .self$lon.name)
      lat  <- ncdf4::ncvar_get(
        .self$mydata, .self$lat.name)
      lon.l <- length(lon)
      lat.l <- length(lat)
      .self$lon.bounds <- c(min(lon), max(lon))
      .self$lat.bounds <- c(min(lat), max(lat))
      if (!.self$conf$is.spatial.avg) {
        .self$spatial.resolution <- 0
      } else {
        .self$spatial.resolution <- abs(lat[2] - lat[1])
      }
      .self$lon360 <- (min(.self$lon.bounds) >= 0 &
                         max(.self$lon.bounds) <= 360)
      if (lon.l == lat.l & !.self$is.spatial.avg) {
        .self$n.spatial <- lon.l
      } else {
        .self$n.spatial <- lon.l * lat.l
      }
      if (.self$time.name == "NA") {
        .self$is.time.indexed <- FALSE
      } else {
        .self$is.time.indexed <- TRUE
        time <- ncdf4::ncvar_get(.self$mydata,
                                 .self$time.name)
        tunits <- strsplit(
          .self$mydata$dim$time$units, " ")[[1]]
        pstart <- as.POSIXlt(tunits[3], tz = "GMT")
        mult <- switch(
          tunits[1],
          "seconds" = 1,
          "minutes" = 60,
          "hours" = 60 ^ 2,
          "days" = 60 ^ 2 * 24,
          default = NA
        )
        .self$start.date <- pstart
        .self$time.units <- tunits[1]
        .self$instants <- as.POSIXlt(pstart + time * mult)
        .self$time.bounds <- as.POSIXlt(pstart +
                                          c(min(time), max(time)) * mult)
        .self$time.mult <- mult
      }
    },

    # ------------------------------------------------------
    # Set methods ------------------------------------------
    # ------------------------------------------------------

    set.lon360 = function(value){
      "Allows users to override the value of lon360,
      which defines whether longitude ranges betweeen
      -180 to 180 (FALSE) or 0 to 360 (TRUE),
      in case the automated algorithm gets it wrong."

      .self$lon360 <- value
    },

    set.terminate = function(){
      prefix <- substr(.self$url, 1, 4)
      if (prefix == "http" | prefix == "loca") {
        ncdf4::nc_close(.self$mydata)
        if (.self$verbose) print("Closed netCDF connection")
      } else {
        #no Jules close?
        if (.self$verbose) print("Nothing to do.")
      }
    },

    set.slice = function(specs = NULL){
      "Takes (a slice from) the input netcdf file associated
      with this object and writes it to a new file.
      If specs are not provided, this method copy-pastes
      the 1st variable to a temporary folder"

      lat <- .self$get.dim.values(.self$lat.name)
      lng360 <- .self$get.dim.values(.self$lon.name)
      lng180 <- (lng360 + 180) %% 360 - 180
      lng360 <- (lng180 + 360) %% 360
      tt <- .self$instants
      if (is.null(specs)) {
        specs <- list(
          name = .self$var.names[1],
          out.fullpath = paste0(tempdir(), "/",
                                .self$var.names[1], ".nc"),
          lon.bounds = lng180[c(1, length(lng180))],
          lat.bounds = lat[c(1, length(lat))],
          time.bounds = tt[c(1, length(tt))]
        )
      }
      x360 <- (specs$lon.bounds + 360) %% 360
      if (!.self$is.time.indexed) {
        tidx <- tbounds <- rep(NA, 2)
      } else if (is.null(specs$time.bounds)) {
        tidx <- c(1, length(tt))
        tbounds <- tt
      } else {
        tidx <- rep(NA, 2)
        lb <- specs$time.bounds[1]
        ub <- specs$time.bounds[2]
        penalty <- function(x){
          same.day <- (x$mday == tt$mday)
          same.month <- (x$mon == tt$mon)
          same.year <- (x$year == tt$year)
          pn <- 10 ^ 20 * (1 - same.year) +
            10 ^ 15 * (1 - same.month) +
            10 ^ 10 * (1 - same.day)
        }
        tidx[1] <- which.min(abs(lb - tt) + penalty(lb))
        tidx[2] <- which.min(abs(ub - tt) + penalty(ub))
        tbounds <- .self$instants[tidx[1]:tidx[2]]
      }
      lon.b.approx <- c(
        lng180[which.min(
          abs(lng180 - specs$lon.bounds[1]) +
            1e10 * (lng180 < specs$lon.bounds[1]))],
        lng180[which.min(
          abs(lng180 - specs$lon.bounds[2]) +
            1e10 * (lng180 > specs$lon.bounds[2]))])
      lat.b.approx <- c(
        lat[which.min(
          abs(lat - specs$lat.bounds[1]) +
            1e10 * (lat < specs$lat.bounds[1]))],
        lat[which.min(
          abs(lat - specs$lat.bounds[2]) +
            1e10 * (lat > specs$lat.bounds[2]))])
      crit.lon <- (lon.b.approx[2] >= lng180 &
                     lon.b.approx[1] <= lng180)
      crit.lat <- (lat.b.approx[2] >= lat &
                     lat.b.approx[1] <= lat)
      lon.r <- lng180[crit.lon]
      lat.r <- lat[crit.lat]
      xb <- (c(lon.r[1], lon.r[length(lon.r)]) + 360) %% 360
      yb <- c(lat.r[1], lat.r[length(lat.r)])

      # extracting data values for the user's
      # Region Of Interest (ROI)
      if (!.self$is.time.indexed) {
        myslice <- .self$iget.2Dslice.grid(
          var.name = specs$name,
          xbounds360 = xb,
          ybounds = yb)
      } else if (!.self$is.spatial.avg) {
        myslice <- .self$iget.2Dslice.station(
          var.name = specs$name, xbounds360 = xb,
          ybounds = yb, tidx = tidx)
        if (length(myslice$value) > 0) {
          # removing stations that don't have a single valid
          # obs for the time slice requested
          crit2 <- apply(Y, MARGIN = 1, FUN = function(vec){
            !all(is.nan(vec))
          })
          nsites <- sum(crit2)
          lon.r <- lon.r[crit2]
          lat.r <- lat.r[crit2]
          myslice$value <- matrix(nr = nsites,
                                  myslice$value[crit2, ])
        }
      } else {
        myslice <- .self$iget.3Dslice(var.name = specs$name,
                                      xbounds360 = xb,
                                      ybounds = yb,
                                      tidx = tidx)
      }
      Y <- myslice$value

      # Exporting netcdf slice to destination file:
      # 1) define dimensions
      londim <- ncdf4::ncdim_def(name = "longitude",
                                 units = "degrees", vals = lon.r)
      latdim <- ncdf4::ncdim_def(name = "latitude",
                                 units = "degrees", vals = lat.r)
      myunits <- .self$var.units[[specs$name]]
      if (.self$is.time.indexed) {
        timedim <- ncdim_def(name = "time",
                             units = .self$time.units,
                             vals = as.numeric(tbounds))
        dim.list <- list(latdim, londim, timedim)
      } else {
        dim.list <- list(latdim, londim)
      }
      # 2) define variable
      if (.self$is.time.indexed) {
        v.def <- ncdf4::ncvar_def(
          name = specs$name,
          units = myunits,
          dim = list(latdim, londim, timedim),
          missval = -9999,
          compression = 9,
          longname = specs$name)
      } else {
        v.def <- ncdf4::ncvar_def(
          name = specs$name,
          units = myunits,
          dim = list(latdim, londim),
          missval = -9999,
          compression = 9,
          longname = specs$name)
      }
      # 3) opening file
      myfile <- specs$out.fullpath
      if (file.exists(myfile)) file.remove(myfile)
      ncout <- ncdf4::nc_create(
        myfile, list(v.def), force_v4 = TRUE)

      # 4) putting data
      ncdf4::ncvar_put(ncout, v.def, Y)

      # 5) writing data to disk and closing the file
      ncdf4::nc_close(ncout)
      rm(ncout)

      return()
    },

    # ------------------------------------------------------
    # Get methods ------------------------------------------
    # ------------------------------------------------------

    get.name = function(){
      "Name of myNetCDF object"

      return(.self$object.name)
    },

    get.summary = function(){
      "URL, variable names, units, descriptions."

      print(.self$mydata)
    },

    get.var.names = function(){
      "List of variable names"

      print(.self$var.names)
    },

    get.dim.values = function(dim.name){
      "Array of values of a specified dimension"

      if (dim.name == "time") {
        out <- .self$instants
      } else {
        out <- ncdf4::ncvar_get(.self$mydata, dim.name)
      }
      return(out)
    },

    get.mydata = function(){
      "Returns data from object"

      dt <- .self$get.slice(bounds = NULL)
      return(dt)
    },

    get.slice = function(bounds = NULL, format = 'lon/lat/time'){

      # Generating a (nlat x nlon) x 2 array of
      # available ERA grid points --> l
      lat <- .self$get.dim.values("latitude")
      lng360 <- .self$get.dim.values("longitude")
      lng180 <- (lng360 + 180) %% 360 - 180
      lng360 <- (lng180 + 360) %% 360
      tt <- .self$instants
      if (is.null(bounds)) {
        bounds <- list(
          lon = lng180[c(1, length(lng180))],
          lat = lat[c(1, length(lat))],
          time = tt[c(1, length(tt))]
        )
      }
      x360 <- (bounds$lon + 360) %% 360
      if (is.null(bounds$time) |
          !.self$is.time.indexed) {
        tidx <- c(1, length(tt))
        tbounds <- tt
      } else {
        tidx <- rep(NA, 2)
        lb <- bounds$time[1]
        ub <- bounds$time[2]
        lb.tt <- tt[1]
        ub.tt <- tt[length(tt)]
        if (lb > ub.tt | ub < lb.tt) {
          tbounds <- c(0, 0)
        } else {
          penalty <- function(x){
            same.day <- (x$mday == tt$mday)
            same.month <- (x$mon == tt$mon)
            same.year <- (x$year == tt$year)
            pn <- 10 ^ 20 * (1 - same.year) +
              10 ^ 15 * (1 - same.month) +
              10 ^ 10 * (1 - same.day)
          }
          tidx[1] <- which.min(abs(lb - tt) + penalty(lb))
          tidx[2] <- which.min(abs(ub - tt) + penalty(ub))
          tbounds <- .self$instants[tidx[1]:tidx[2]]
        }
      }
      if (bounds$lon[2] < min(lng180) | bounds$lon[1] > max(lng180) |
          bounds$lat[2] < min(lat) | bounds$lat[1] > max(lat) |
          identical(tbounds, c(0,0))) {
        return(NULL)
      }
      if (!.self$is.spatial.avg) {
        l <- cbind(lng180, lat)
      } else {
        l <- expand.grid(lng180, lat)
      }
      lon.b.approx <- c(
        lng180[which.min(abs(
          lng180 - bounds$lon[1]) +
            1e10 * (lng180 < bounds$lon[1]))],
        lng180[which.min(abs(
          lng180 - bounds$lon[2]) +
            1e10 * (lng180 > bounds$lon[2]))])
      lat.b.approx <- c(
        lat[which.min(
          abs(lat - bounds$lat[1]) +
            1e10 * (lat < bounds$lat[1]))],
        lat[which.min(abs(
          lat - bounds$lat[2]) +
            1e10 * (lat > bounds$lat[2]))])
      crit1 <- (lon.b.approx[2] >= l[, 1]) &
        (lon.b.approx[1] <= l[, 1]) &
        (lat.b.approx[2] >= l[, 2]) &
        (lat.b.approx[1] <= l[, 2])
      lon.r  <- l[crit1, 1]
      lat.r  <- l[crit1, 2]
      xb <- (c(min(lon.r), max(lon.r)) + 360) %% 360
      yb <- c(min(lat.r), max(lat.r))

      # extracting variable values for
      # the user's Region Of Interest (ROI)
      if (.self$is.spatial.avg & .self$is.time.indexed) {
        myslice <- .self$iget.3Dslice(
          var.name = .self$conf$var.name,
          xbounds360 = xb, ybounds = yb, tidx = tidx,
          format = format)
      } else if (.self$is.spatial.avg &
                 !.self$is.time.indexed) {
        myslice <- .self$iget.2Dslice.grid(
          var.name = .self$conf$var.name,
          xbounds360 = xb, ybounds = yb)
      } else if (!.self$is.spatial.avg &
                 .self$is.time.indexed) {
        tmp <- .self$iget.2Dslice.station(
          var.name = .self$conf$var.name,
          xbounds360 = xb, ybounds = yb, tidx = tidx)
        if (length(tmp) > 0) {
          # removing stations that don't have a single valid
          # obs for the time slice requested
          crit2 <- apply(
            tmp, MARGIN = 1, FUN = function(vec) {
              !all(is.nan(vec))
            })
          nsites <- sum(crit2)
          myslice <- list(
            lon = lon.r[crit2],
            lat = lat.r[crit2],
            time = tbounds,
            value = matrix(nr = nsites, tmp[crit2, ]))
        }
      } else {
        stop("Not implemented yet")
      }
      return(myslice)
    },

    get.xyz = function(
      x.name = "lon", y.name = "lat", z.name,
      include.z = NULL, subsample.treshold = 1000,
      bounds = NULL){
      "Provides a 3-column data frame (x,y,z) from a netcdf
      file, e.g. for plotting. Subsamples if needed."

      dt <- .self$get.slice(bounds = bounds)
      allz <- as.matrix(dt$value)
      nx <- length(dt[[x.name]])
      ny <- length(dt[[y.name]])
      n <- min(nx, ny)
      if (n > subsample.treshold) {
        substep <- floor(n / subsample.treshold)
        idx.x <- seq(1, nx, by = substep)
        idx.y <- seq(1, ny, by = substep)
        l <- expand.grid(
          x = as.numeric(dt[[x.name]][idx.x]),
          y = rev(as.numeric(dt[[y.name]][idx.y])))
        z <- as.numeric(t(allz[idx.y, idx.x]))
      } else {
        l <- expand.grid(
          x = as.numeric(dt[[x.name]]),
          y = rev(as.numeric(dt[[y.name]])))
        z <- as.numeric(t(allz))
      }
      linc <- length(include.z)
      if (linc == 0) {
        mydf <- data.frame(x = l$x, y = l$y, z = z)
      } else if (linc == 1) {
        crit <- (z == include.z)
        mydf <- data.frame(
          x = l$x[crit], y = l$y[crit], z = z[crit])
      } else if (linc == 2) {
        crit <- (z >= include.z[1] & z <= include.z[2])
        mydf <- data.frame(
          x = l$x[crit], y = l$y[crit], z = z[crit])
      }
      return(mydf)
    },

    # ------------------------------------------------------
    # Internal methods -------------------------------------
    # ------------------------------------------------------

    iget.2Dslice.station = function(var.name = NULL,
                                    xbounds360=NULL,
                                    ybounds = NULL,
                                    tidx = NULL){
      "Get a slice of data from a specified 2D variable
      (time and location index)"

      if (is.null(var.name)) {
        vname <- .self$var.names[1]
      } else {
        vname <- var.name
      }
      bnds <- .self$iget.spatial.bounds(
        xbounds360 = xbounds360, ybounds = ybounds)
      # Getting the full data first,
      # for the specified instant(s)
      myfulldata <- ncdf4::ncvar_get(
        .self$mydata, varid = vname,
        start = c(tidx[1], 1),
        count = c(tidx[2] - tidx[1] + 1, .self$n.spatial))
      #slicing locally
      lat  <- ncvar_get(.self$mydata, .self$lat.name)
      lon180 <- (ncdf4::ncvar_get(
        .self$mydata, .self$lon.name) + 180) %% 360 -
        180
      cr <- (lon180 >= bnds$x[1] & lon180 <= bnds$x[2] &
               lat >= bnds$y[1] & lat <= bnds$y[2])
      if (tidx[2] > tidx[1]) {
        value <- t(myfulldata[, cr])
      } else {
        value <- matrix(nc = 1, myfulldata[cr])
      }
      myslice <- list(lat = lat[cr], lon = lon[cr],
                      value = value)
      return(myslice)
    },

    iget.2Dslice.grid = function(var.name = NULL,
                                 xbounds360 = NULL,
                                 ybounds = NULL){
      "Get slice of data from 2D variable (lat/lon)"

      if (is.null(var.name)) {
        vname <- .self$var.names[1]
      } else {
        vname <- var.name
      }
      bnds <- .self$iget.spatial.bounds(
        xbounds360 = xbounds360, ybounds = ybounds)
      lon <- ncdf4::ncvar_get(.self$mydata,
                              .self$lon.name)
      lat <- rev(ncdf4::ncvar_get(
        .self$mydata, .self$lat.name))
      xidx.unsorted <- c(which.min(abs(
        lon - bnds$x[1] +
          1e10 * (lon - bnds$x[1] < -1 / 240))),
        which.min(abs(
          lon - bnds$x[2] +
            1e10 * (lon - bnds$x[2] > 1 / 240))))
      yidx.unsorted <- c(
        which.min(abs(
          lat - bnds$y[1] +
            1e10 * (lat - bnds$y[1] < -1 / 240))),
        which.min(abs(
          lat - bnds$y[2] +
            1e10 * (lat - bnds$y[2] > 1 / 240))))
      xidx <- sort(xidx.unsorted)
      yidx <- sort(yidx.unsorted)
      value <- ncdf4::ncvar_get(
        .self$mydata,
        varid = vname,
        start = c(yidx[1], xidx[1]),
        count = c(yidx[2] - yidx[1] + 1,
                  xidx[2] - xidx[1] + 1))
      myslice <- list(
        lat = lat[yidx.unsorted[1]:yidx.unsorted[2]],
        lon = lon[xidx.unsorted[1]:xidx.unsorted[2]],
        value = value)
      return(myslice)
    },

    iget.3Dslice = function(var.name = NULL,
                            xbounds360 = NULL,
                            ybounds = NULL,
                            tidx = NULL,
                            format = 'lon/lat/time') {
      "Get slice of data from a 3D variable. Formats:
      lon/lat/time: default;
      lat/lon/time: more amenable for plotting matrix;
      revlat/lon/time: best for plotting Matrix data."

      if (is.null(var.name)) {
        vname <- .self$var.names[1]
      } else {
        vname <- var.name
      }
      bnds <- .self$iget.spatial.bounds(
        xbounds360 = xbounds360, ybounds = ybounds)
      lon <- ncdf4::ncvar_get(.self$mydata,
                              .self$lon.name)
      lat <- ncdf4::ncvar_get(.self$mydata,
                              .self$lat.name)
      xidx.unsorted <- c(
        which.min(abs(
          lon - bnds$x[1] + 1e10 * (lon < bnds$x[1]))),
        which.min(abs(lon - bnds$x[2] +
                        1e10 * (lon > bnds$x[2]))))
      yidx.unsorted <- c(
        which.min(abs(lat - bnds$y[1] +
                        1e10 * (lat < bnds$y[1]))),
        which.min(abs(lat - bnds$y[2] +
                        1e10 * (lat > bnds$y[2]))))
      xidx <- sort(xidx.unsorted)
      yidx <- sort(yidx.unsorted)
      nx <- xidx[2] - xidx[1] + 1
      ny <- yidx[2] - yidx[1] + 1
      np <- nx * ny
      nt <- tidx[2] - tidx[1] + 1
      #Getting the data slice
      mydt <- ncdf4::ncvar_get(.self$mydata, varid = vname,
        start = c(xidx[1], yidx[1], tidx[1]), count = c(nx, ny, nt))
      value <- switch(
        format,
        'lon/lat/time' = matrix(nr = np, nc = nt, mydt),
        'lat/lon/time' = matrix(nr = np, nc = nt, aperm(aperm, c(2,1,3))),
        'revlat/lon/time' = {
          ord <- as.numeric(t(mapply(1:ny, FUN = function(j) {
            seq(j, ny * nx, by = ny)
          })))
          mat <- matrix(nr = np, nc = nt, aperm(aperm, c(2,1,3)))
          mat[ord, ]
        }
      )
      if (format == 'revlat/lon/time') {
        aux <- lat[yidx.unsorted[2]:yidx.unsorted[1]]
      } else {
        aux <- lat[yidx.unsorted[1]:yidx.unsorted[2]]
      }
      myslice <- list(lat = aux,
                      lon = lon[xidx.unsorted[1]:xidx.unsorted[2]],
                      value = value)
      return(myslice)
    },

    iget.spatial.bounds = function(xbounds360, ybounds){
      "Sets up spatial bounds for slicing"

      # Longitude calculations
      if (.self$lon360) {
        if (is.null(xbounds360)) {
          xb <- .self$lon.bounds
        } else {
          xb <- xbounds360
        }
      } else {
        if (is.null(xbounds360)) {
          xbnd <- .self$lon.bounds
        } else {
          xbnd <- xbounds360
        }
        xb <- (xbnd + 180) %% 360 - 180
      }

      # Latitude calculations
      if (is.null(ybounds)) {
        yb <- .self$lat.bounds
      } else {
        yb <- c(max(ybounds[1], .self$lat.bounds[1]),
                min(ybounds[2], .self$lat.bounds[2]))
      }
      out <- list(x = xb, y = yb)
      return(out)
    },

    iget.time.indices = function(tbounds){
      "Sets up time bound indices for slicing"

      if (is.null(tbounds)) {
        tidx <- c(1, length(.self$instants))
      } else if (is.character(tbounds[1]) &
                 is.character(tbounds[2])) {
        tidx <- as.numeric(
          mapply(tbounds, FUN = function(mytime){
            dt <- mytime
            idx <- which.min(abs(dt - .self$instants))
            return(idx)
          }))
      } else {
        stop("Unknown input for tbounds.")
      }
      return(tidx)
    }
  )
)
