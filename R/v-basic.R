#' Basic virtual reference class
#'
#' Virtual ("template") Reference Class for all RCs
#'
#' This reference class contains fields (aka "attributes")
#' and methods (aka "procedures") for all basic RCs.
#'
#' @field package.name character. Name of package
#' @field object.name character. Name of object
#' @field verbose logical. Are methods verbose when called?
#' @field conf list. Object configuration
#'
#' @import ncdf4
#' @exportClass rcvirtual.basic
#'
setRefClass(
  Class = "rcvirtual.basic",
  contains = c("VIRTUAL"),
  fields = list(
    package.name = "character",
    object.name = "character",
    conf = "list",
    verbose = "logical"
  ),
  methods = list(

    # ------------------------------------------------------
    # Initializer methods ----------------------------------
    # ------------------------------------------------------

    initialize = function(package.name = "anonymous",
                          object.name = "anonymous",
                          verbose = TRUE,
                          autoconstruct = FALSE,
                          conf = NULL) {
      "Default method to initialize basic objects"

      .self$package.name <- package.name
      .self$object.name <- object.name
      .self$verbose <- verbose
      if (is.list(conf)) .self$set.conf(conf)
      if (autoconstruct) .self$construct()
    },

    construct = function() {
      "Construct basic objects"

      if (.self$verbose) {
        cat("Constructing object", object.name,
            "for package ", .self$package.name, "\n")
      }
    },

    # ------------------------------------------------------
    # Set methods ------------------------------------------
    # ------------------------------------------------------

    set.conf = function(conf = NULL) {
      "Sets the configuration list for this object"

      .self$conf <- switch(
        class(conf),
        "list" = conf,
        "data.frame" = as.list(conf)
      )
    },

    set.verbose = function(verbose) {
      "Changes verbosity level"

      .self$verbose <- verbose
    },

    set.name = function(new.name){
      "Sets the name of a random variable/vector."

      q <- paste0("Changed the name of ",
                  class(.self),
                  ", from '", .self$object.name,
                  "' to '", new.name,"'")
      print(q, quote = FALSE)
      .self$object.name <- new.name
    },

    # ------------------------------------------------------
    # Get methods ------------------------------------------
    # ------------------------------------------------------

    get.name = function(){
      "Provides the object's name."

      q <- paste0("Name of ", class(.self), ": '",
                  .self$object.name, "'")
      print(q, quote = FALSE)
    },

    get.rdata = function(fullpath) {
      "Reads an .RData file and passes
      its contents as a list"

      load(fullpath)
      nm <- objects()
      nm <- nm[nm != "fullpath"]
      out <- lapply(nm, FUN = function(x) get(x))
      if (length(nm) == 1) {
        out <- out[[1]]
      } else {
        names(out) <- nm
      }
      return(out)
    },

    get.txt = function(fullpath) {
      "Reads a .txt file and passes
      its contents as a dataframe"

      out <- read.table(fullpath, header = TRUE)
      return(out)
    },

    get.csv = function(fullpath, unlist = FALSE){
      "General method to import comma separated value files"

      mydt <- read.csv(
        fullpath, header = TRUE, stringsAsFactors = FALSE
      )
      if (unlist) {
        mydt <- if (is.list(mydt)) unlist(mydt)
      }
      return(mydt)
    },

    get.netcdf = function(fullpath, var.name = NULL){
      "Retrieves data from any netcdf file on disk and
      returns a list"

      a <- ncdf4::nc_open(fullpath)
      if (is.null(var.name)) {
        vname <- a$var.names[1]
      } else {
        vname <- var.name
      }
      filedata <- vector("list", length = a$ndims + a$nvars)
      for (i in 1:a$ndims) {
        filedata[[i]] <- a$dim[[i]]$vals
      }
      for (j in 1:a$nvars) {
        i <- a$ndims + j
        myid <- names(a$var)[j]
        filedata[[i]] <- ncdf4::ncvar_get(a, varid = myid)
      }
      names(filedata) <- c(names(a$dim), names(a$var))
      return(filedata)
    },

    get.conf = function(id = NULL, field = NULL) {
      "Provides the entire configuration object, a vector of
      entries specified by id, or just a single entry
      specified by id and field."

      if (is.null(id)) {
        res <- .self$conf
      } else {
        if (!is.null(.self$conf[[id]])) {
          myarray <- .self$conf[[id]]
        } else if (!is.null(.self$conf[[id]])) {
          myarray <- .self$conf[[id]]
        }
        if (!is.null(field)) {
          res <- myarray[field]
        } else {
          res <- myarray
        }
      }
      return(res)
    },

    # ------------------------------------------------------
    # Is methods -------------------------------------------
    # ------------------------------------------------------

    # ------------------------------------------------------
    # User methods -----------------------------------------
    # ------------------------------------------------------

    fields = function() {
      "Lists the fields available in this object"

      get(class(.self)[1])$fields()
    },

    methods = function() {
      "Lists the methods available in this object"

      r5methods <- c("callSuper", "copy",
                     "export", "field", "getClass",
                     "getRefClass", "import", "initFields",
                     "show", "trace", "untrace",
                     "usingMethods", ".objectPackage",
                     ".objectParent")
      all.methods <- get(class(.self)[1])$methods()
      sub.crit <- mapply(all.methods, FUN = function(x){
        all(x != r5methods)
      })
      sub.methods <- all.methods[sub.crit]
      up.crit <- mapply(sub.methods, FUN = function(x){
        grepl("#", x)
      })
      up.methods <- sub.methods[up.crit]
      my.methods <- sub.methods[!up.crit]
      out <- list(exclusive = my.methods,
                  inherited = up.methods,
                  general = r5methods)
      return(out)
    },

    help = function(method = .self$methods()) {
      "Prints the description under a specific method"

      get(class(.self)[1])$help(as.character(method))
    },

    validate = function() {
      "Validate basic objects"

      if (.self$verbose) {
        cat("Validating object", .self$object.name, "\n")
      }
    }

  )
)