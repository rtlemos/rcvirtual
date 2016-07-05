#' rcvirtual.conf: Object configuration
#'
#' @field daemon. Specifications for daemon
#' @field parameters. Specifications for parameters
#' @field strategy. Specifications for strategy
#' @field plotter. Specifications for plotter
#'
#' @importFrom methods new
#' @exportClass rcvirtual.conf
#'
setRefClass(
  Class = "rcvirtual.conf",
  contains = c("rcvirtual.basic", "VIRTUAL"),
  fields = list(daemon = "ANY",
                parameters = "ANY",
                strategy = "ANY",
                plotter = "ANY"
  ),
  methods = list(

    # ------------------------------------------------------
    # Initializer methods ----------------------------------
    # ------------------------------------------------------

    construct = function(){
      "Constructor of virtual conf"

      callSuper()

      x <- vector("list", length = 0)
      .self$daemon <- x
      .self$parameters <- x
      .self$strategy <- x
      .self$plotter <- x
    },

    # ------------------------------------------------------
    # Set methods ------------------------------------------
    # ------------------------------------------------------


    # ------------------------------------------------------
    # Get methods ------------------------------------------
    # ------------------------------------------------------

    get.conf.template = function(){
      name.vec <- c(
        "alpha", "beta", "gamma", "delta", "epsilon",
        "zeta", "eta", "theta", "iota", "kappa", "lambda",
        "mu", "nu", "xi", "omicron", "pi", "rho", "sigma",
        "tau", "upsilon", "phi", "chi", "psi", "omega",
        "varphi", "varpi", "varrho", "vartheta", "varsigma",
        "varepsilon", "ALPHA", "BETA", "GAMMA", "DELTA",
        "EPSILON", "ZETA", "ETA", "THETA", "IOTA", "KAPPA",
        "LAMBDA", "MU", "NU","XI","OMICRON", "PI", "RHO",
        "SIGMA", "TAU", "UPSILON", "PHI", "CHI", "PSI",
        "OMEGA", letters, LETTERS)
      ll <- length(name.vec)
      na.ini <- rep(NA, ll)
      d.ini <- rep("-", ll)
      o.ini <- rep(1, ll)
      f.ini <- rep(FALSE, ll)
      t.ini <- rep(TRUE, ll)
      tpl <- data.frame(
        name = d.ini, #variable name, or 1-letter codename
        long.name = d.ini, #long variable name
        type = d.ini, #fixed, unknown, derived
        lbound = na.ini, #lower bound
        ubound = na.ini, #upper bound
        units = d.ini, #units of parameter
        prior.distr = d.ini, #prior distribution codename
        prior.mean = na.ini, #prior mean
        prior.var = na.ini, #prior variance
        sampler.type = d.ini, #type of sampler used
        proposal.distr = d.ini, #prop. distribution codename
        proposal.var = na.ini, #proposal variance
        accept.rate = na.ini, #acceptance rate in M-H algor.
        initial = na.ini, #initial value
        is.spatial.avg = f.ini, #is this a spatial average?
        size = o.ini, #size of vector parameter
        input.file = d.ini, #input file
        output.file = d.ini, #output file
        store.in.ram = t.ini,
        stringsAsFactors = FALSE)
      tpl$name <- name.vec
      return(tpl)
    }

    # ------------------------------------------------------
    # Is methods -------------------------------------------
    # ------------------------------------------------------


  )
)
