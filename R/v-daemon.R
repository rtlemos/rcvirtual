#' Controller (daemon) of statistical models
#'
#' This is a virtual Reference Class for concrete daemon RCs
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any daemon RC must have.
#'
#' @field parameters. Parameters object
#' @field strategy. Strategy object
#' @field plotter. Plotter object
#'
#' @importFrom methods new
#' @exportClass rcvirtual.daemon
#'
setRefClass(
  Class = "rcvirtual.daemon",
  contains = c("rcvirtual.basic", "VIRTUAL"),
  fields = list(parameters = "rcvirtual.parameters",
                strategy = "rcvirtual.strategy",
                plotter = "rcvirtual.plotter"),
  methods = list(

    # ------------------------------------------------------
    # Initializer methods ----------------------------------
    # ------------------------------------------------------
    initialize = function(object.name = NULL,
                          verbose = TRUE,
                          autoconstruct = FALSE,
                          conf = NULL,
                          use.gui = FALSE){
      "Initialize the daemon"

      # - Initializing the Daemon -------------------------

      package <- (.self$getClass())@className[1]
      if (is.null(object.name)) {
        object <- paste0(package, ".daemon")
      } else {
        object <- object.name
      }

      #If user doesn't provide config, use default
      conf.name <- paste0(package, ".conf")
      if (is.null(conf)) {
        uconf <- get(conf.name)$new(package.name = package,
                                    object.name = paste0(package, '.conf'))
        uconf$construct()
      } else {
        if (!is(conf, "rcvirtual.conf")) {
          stop("You must provide an offspring of rcvirtual.conf")
        } else {
          uconf <- conf
        }
      }

      # trim uconf, keeping only quantities that enter the model
      uconf$set.trim()

      # deploying the Shiny app per user request, to modify uconf
      if (use.gui) {
        gui.name <- paste0(package, '.guiconf')
        gg <- get(gui.name)$new(uconf = uconf)
        uconf <- gg$launch.app()
      }

      # finilizing this daemon's setup, by calling the Basic setup
      callSuper(package.name = package,
                object.name = object,
                verbose = verbose,
                autoconstruct = FALSE,
                conf = uconf$daemon)

      # - Initializing dependencies -----------------------

      parameters.name <- paste0(package, ".parameters")
      strategy.name <- paste0(package, ".strategy")
      plotter.name <- paste0(package, ".plotter")

      .self$parameters <- get(parameters.name)$new(
        package.name = package,
        object.name = parameters.name,
        conf = uconf$parameters)
      .self$strategy <- get(strategy.name)$new(
        package.name = package,
        object.name = strategy.name,
        conf = uconf$strategy)
      .self$plotter <- get(plotter.name)$new(
        package.name = package,
        object.name = plotter.name,
        conf = uconf$plotter)

      # linking parameters to strategy and plotter
      .self$strategy$parameters <- .self$parameters
      .self$plotter$parameters <- .self$parameters

      # moving data sets in package to *.Rdata objects in tempdir()
      for (obj in as.character(data(package = package)$results[, "Item"])) {
        save(obj, file = paste0(tempdir(), '/', obj, '.RData'))
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

      .self$parameters$construct()
      if (where.stop == "parameters") return()
      .self$strategy$construct()

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

      if (.self$parameters$is.valid.parameter(name)) {
        mydata <- .self$parameters$get.data(
          param.name = name, field.name = "value")
      } else {
        mydata <- NULL
      }
      return(mydata)
    },

    # ------------------------------------------------------
    # Plotting methods -------------------------------------
    # ------------------------------------------------------
    graphplot = function(highlight.node.name = NULL, highlight.edges = 'to') {
      'Plot a graph of model parameters, highlighting a particular node
      and any edges that point to/from it'

      graph <- .self$strategy$get.ordered.graph()
      .self$plotter$graphplot(graph, highlight.node.name, highlight.edges)
      .self$plotter$get.buffer.plot()
    },

    # ------------------------------------------------------
    # User methods -----------------------------------------
    # ------------------------------------------------------

    fit = function(mle = TRUE){
      "Request the daemon to fit a model"

      # i) initializing derived quantities
      .self$parameters$set.initial()
      .self$parameters$is.valid()

      # ii) initializing strategy
      .self$strategy$construct()
      .self$strategy$set.initial()
      .self$strategy$is.valid()

      # iii) getting mle
      .self$strategy$set.optimize(mle = mle)
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
