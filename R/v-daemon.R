#' Controller (daemon) of statistical models
#'
#' This is a virtual Reference Class for concrete daemon RCs
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") that any daemon RC must have.
#'
#' @field strategy. Strategy object
#' @field plotter. Plotter object
#'
#' @include v-basic.R v-strategy.R v-plotter.R
#' @importFrom methods new
#' @exportClass rcvirtual.daemon
#'
setRefClass(
  Class = "rcvirtual.daemon",
  contains = c("rcvirtual.basic", "VIRTUAL"),
  fields = list(strategy = "rcvirtual.strategy",
                plotter = "rcvirtual.plotter"),
  methods = list(

    # ------------------------------------------------------
    # Initializer methods ----------------------------------
    # ------------------------------------------------------
    initialize = function(object.name = NULL,
                          verbose = TRUE,
                          autoconstruct = FALSE,
                          use.gui = FALSE){
      "Initialize the daemon"

      # - Initializing the Daemon -------------------------

      package <- (.self$getClass())@className[1]
      if (is.null(object.name)) {
        object <- paste0(package, ".daemon")
      } else {
        object <- object.name
      }

      # finilizing this daemon's setup, by calling the Basic setup
      callSuper(package.name = package,
                object.name = object,
                verbose = verbose,
                autoconstruct = FALSE)

      # - Initializing dependencies -----------------------

      strategy.name <- paste0(package, ".strategy")
      plotter.name <- paste0(package, ".plotter")

      .self$strategy <- get(strategy.name)$new(
        package.name = package,
        object.name = strategy.name)
      .self$plotter <- get(plotter.name)$new(
        package.name = package,
        object.name = plotter.name)

      # linking strategy and plotter
      .self$plotter$strategy <- .self$strategy

      # moving data sets in package to *.Rdata objects in tempdir()
      if (!is.null(tryCatch(data(package = package), error = function(e) NULL))) {
        for (obj in as.character(data(package = package)$results[, "Item"])) {
          save(list = obj, file = paste0(tempdir(), '/', obj, '.RData'))
        }
      }

      # If autoconstruct = TRUE, proceed to constructing daemon and dependencies
      if (autoconstruct) {
        .self$construct()
      }

    },

    construct = function(where.stop = "end"){
      "Constructor of daemon RCs"

      # Constructing the daemon's Basic objects
      callSuper()

      .self$strategy$construct()
      if (where.stop == "strategy") return()
      .self$plotter$construct()

    },

    # ------------------------------------------------------
    # Set methods ------------------------------------------
    # ------------------------------------------------------

    # ------------------------------------------------------
    # Get methods ------------------------------------------
    # ------------------------------------------------------

    get.data = function(name){
      "Retrieve data, stored in memory and controlled
      by the daemon"

      mydata <- .self$strategy$get.data(
        param.name = name, field.name = "value")
      return(mydata)
    },

    # ------------------------------------------------------
    # Plotting methods -------------------------------------
    # ------------------------------------------------------
    graphplot = function(highlight.node.name = NULL, highlight.edges = 'to',
                         col = NULL) {
      'Plot a graph of model parameters, highlighting a particular node
      and any edges that point to/from it'

      .self$plotter$graphplot(.self$strategy$graph,
                              .self$strategy$get.parameter.types(),
                              highlight.node.name, highlight.edges, col)
      .self$plotter$get.buffer.plot()
    },

    # ------------------------------------------------------
    # User methods -----------------------------------------
    # ------------------------------------------------------

    fit = function(mle = FALSE, maxit = 1000){
      "Request the daemon to fit a model"

      .self$strategy$set.optimize(mle = mle, maxit = maxit)
    },

    vignette = function(package.name = .self$name){
      "Provides the vignette associated with this package"

      vname <- paste0(package.name, "-vignette")
      utils::vignette(vname, package.name)
    },

    clean.tmp = function(){
      "Clean the temporary folder"

      unlink(tempdir(), recursive = FALSE)
    }
  )
)
