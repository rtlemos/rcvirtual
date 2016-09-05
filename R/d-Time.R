#' Time objects
#'
#' This is a Reference Class for objects that handle time information
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any random RC must have.
#'
#' @include v-basic.R
#' @importFrom methods new
#' @export Time
#' @exportClass Time
#'
Time <- setRefClass(
  Class = "Time",
  contains = c("rcvirtual.basic"),
  fields = list(type = 'character',
                size = 'numeric',
                value = 'numeric',
                date = 'POSIXlt',
                cyclical.instant = 'numeric',
                step = 'character',
                day = 'numeric',
                month = 'numeric',
                year = 'numeric',
                lb = 'numeric',
                ub = 'numeric'
                ),
  methods = list(
    initialize = function(lb, ub, tz = 'GMT', step = 'days', name = 'time') {

      #
      # Computations for the name
      #
      .self$object.name <- name
      .self$type <- 'Time'
      #
      # Computations for bounds
      #
      stopifnot(as.POSIXct(lb, tz) <= as.POSIXct(ub, tz))
      tb <- as.POSIXlt(x = c(lb, ub), tz = tz)
      .self$step <- step
      if (step == "day" | step == "days") {
        stp <- 60 ^ 2 * 24
        n <- 1 + as.numeric(difftime(time1 = tb[2],
                                     time2 = tb[1],
                                     tz = tz, units = 'days'))
        .self$value <- seq(1, n, 1)
        aux <- as.POSIXlt(seq(tb[1], tb[2], by = stp))
        .self$day <- aux$mday
        .self$month <- aux$mon + 1
        .self$year <- 1900 + aux$year
        .self$cyclical.instant <- mapply(1:n, FUN = function(i) {
          as.numeric(strftime(paste(.self$year[i], .self$month[i],
                                    .self$day[i], sep = "-"), format = "%j"))
        })
      } else if (step == "month" | step == "months") {
        n <- (tb[2]$year - tb[1]$year) * 12 + tb[2]$mon - tb[1]$mon + 1
        .self$value <- seq(1, n, 1)
        .self$day <- rep(1, n)
        .self$month <- (tb[1]$mon + inst - 1) %% 12 + 1
        .self$year <- 1900 + tb[1]$year +
          floor((tb[1]$mon + .self$value - 1) / 12)
        .self$cyclical.instant <- .self$month
      } else if (step == "year" | step == "years") {
        n <- tb[2]$year - tb[1]$year + 1
        .self$value <- seq(1, n, 1)
        .self$day <- rep(1, n)
        .self$month <- rep(1, n)
        .self$year <- 1900 + seq(tb[1]$year, tb[2]$year)
        .self$cyclical.instant <- rep(1, n)
      } else {
        stop("Timestep not recognized", step)
      }
      .self$date <- as.POSIXlt(mapply(.self$year, .self$month, .self$day,
                                       FUN = function(y, m, d) {
                                         paste(y, m, d, sep = "/")
                                       }))
      .self$lb <- 1
      .self$ub <- n
      .self$size <- n
    }
  )
)
