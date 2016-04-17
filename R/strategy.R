#' Statistical model-fitting algorithms (strategy)
#'
#' This is a virtual Reference Class for concrete strategy RCs
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any strategy RC must have.
#'
#' @exportClass rcvirtual.strategy
#'
setRefClass(
    Class = "rcvirtual.strategy",
    contains = c("rcvirtual.basic", "VIRTUAL"),
    fields = list(parameters = "rcvirtual.parameters",
                  model.fitted = "logical"),
    methods = list(

        construct = function(specs) {
            "Construct a new strategy object"

            callSuper(specs)

            #placing pointer to parameters in strategy
            .self$parameters <- specs$parameters

            .self$set.model.fitted(FALSE)

        },

        validate = function() {
            "Function that checks if strategy is valid."

            if (.self$verbose) print("Validating strategy...")

            #TODO
        },

        set.model.fitted = function(status){
            "Change the status of the model (fitted vs. not fitted)"

            .self$model.fitted <- status
        },

        is.model.fitted = function(){
            "Check if the model has been fitted"

            return(.self$model.fitted)
        }
    )
)
