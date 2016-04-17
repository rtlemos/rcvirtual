#' Basic virtual reference class
#'
#' Virtual ("template") Reference Class for all reference classes
#'
#' This reference class contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for all basic RCs.
#'
#' @field name character. Name of object
#' @field verbose logical. Should methods be verbose when they run?
#'
#' @importFrom methods setRefClass
#'
#' @exportClass rcvirtual.basic
#'
setRefClass(
  Class = "rcvirtual.basic",
  contains = c("VIRTUAL"),
  fields = list(
    name = "character",
    verbose = "logical"
  ),
  methods = list(
    fields = function() {
      "Lists the fields available in this object"

      get(class(.self)[1])$fields()
    },

    methods = function() {
      "Lists the methods available in this object"

      get(class(.self)[1])$methods()
    },

    help = function(method = .self$methods()) {
      "Prints the description under a specific method"

      get(class(.self)[1])$help(as.character(method))
    },

    initialize = function(name = "anonymous", verbose = TRUE,
                          autoconstruct = FALSE, specs = NULL) {
      "Default method to initialize basic objects"

      .self$name <- name
      .self$verbose <- verbose
      if (autoconstruct) .self$construct(specs = specs)
    },

    construct = function(specs) {
      "Construct basic objects"

      if (.self$verbose) {
        cat("Constructing object", .self$name, "\n")
      }
    },

    activate = function(i) {
      "Lists available methods, other than standard ones"

      r5methods <- c("activate", "callSuper", "copy", "export", "field",
                     "getClass", "getRefClass", "import", "initFields",
                     "show", "trace", "untrace", "usingMethods")
      fvec <- get(cl)$methods()
      if (!(any(fvec[i] == r5methods))) {
        get(paste(".self$", fvec[i], "(touch=TRUE)", sep = ""))
      }
    },

    set.verbose = function(verbose) {
      "Changes verbosity level"

      .self$verbose <- verbose
    }
  )
)
