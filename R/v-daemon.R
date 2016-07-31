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
    initialize = function(package.name,
                          object.name = ".daemon",
                          verbose = TRUE,
                          autoconstruct = FALSE,
                          conf = NULL,
                          use.gui = FALSE){
      "Initialize the daemon"

      #If user doesn't provide config, use default
      conf.name <- paste0(package.name, ".conf")
      if (!is.null(conf)) {
        if (!is(conf, "rcvirtual.conf")) {
          stop("You must provide an offspring of
               rcvirtual.conf")
        } else {
          uconf <- conf
        }
      } else {
        uconf <- get(conf.name)$new()
      }

      # trim uconf, keeping only quantities that enter the model
      uconf$set.trim()

      # deploying the Shiny app if the user requested it, to modify uconf
      if (use.gui) {
        gui.name <- paste0(package.name, '.gui')
        gg <- get(gui.name)$new(conf = uconf)
        uconf <- gg$launch.app()
      }

      parameters.name <- paste0(package.name, ".parameters")
      strategy.name <- paste0(package.name, ".strategy")
      plotter.name <- paste0(package.name, ".plotter")

      callSuper(package.name = package.name,
                object.name = object.name,
                verbose = verbose,
                autoconstruct = autoconstruct,
                conf = uconf$daemon)

      .self$parameters <- get(parameters.name)$new(
        package.name = package.name,
        object.name = parameters.name,
        conf = uconf$parameters)
      .self$strategy <- get(strategy.name)$new(
        package.name = package.name,
        object.name = strategy.name,
        conf = uconf$strategy)
      .self$plotter <- get(plotter.name)$new(
        package.name = package.name,
        object.name = plotter.name,
        conf = uconf$plotter)

      #.self$dataset <- lapply(
      #  1:length(uconf$dataset$name), FUN = function(i){
      #    get(dataset.name)$new(
      #      name = uconf$dataset$name[i],
      #      conf = as.list(uconf$dataset[i, ]))
      #  })

      #linking parameters, strategy, dataset & plotter
      .self$strategy$parameters <- .self$parameters
      .self$plotter$parameters <- .self$parameters

      #saving package data into temp folder
      #if (length(uconf$datasets$names) > 0) {
      #  for (i in 1:length(uconf$datasets$names)) {
      #    dname <- uconf$datasets$names[i]
      #    oname <- paste0(tempdir(), "/", dname, ".Rdata")
      #    save(dname, file = oname)
      #  }
      #}
      },

    construct = function(where.stop = "end"){
      "Constructor of daemon RCs"

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
