#' Graphics controller (plotter) of statistical models
#'
#' This is a virtual Reference Class for concrete plotter RCs
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any plotter RC must have.
#'
#' @exportClass rcvirtual.plotter
#'
setRefClass(
    Class = "rcvirtual.plotter",
    contains = c("rcvirtual.basic", "VIRTUAL"),
    fields = list(buffer = "matrix",
                  palettes = "list",
                  default.palettes = "list"),
    methods = list(
        construct = function(specs){
            "Constructs the printer object"

            callSuper(specs)

            #default palettes are colour-blind friendly
            dfp <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                     "#0072B2", "#D55E00", "#CC79A7")
            .self$default.palettes <- list(
                line = dfp,
                fill = dfp,
                CI = c(0.8, 0.7),
                neg.zero.pos = c("blue", "white", "red"),
                zero.pos = c("white", "black"))
            .self$palettes <- list(
                line = .self$default.palettes$line,
                fill = .self$default.palettes$fill,
                CI = .self$default.palettes$CI,
                neg.zero.pos = .self$default.palettes$neg.zero.pos,
                zero.pos = .self$default.palettes$zero.pos
            )
        },

        set.buffer.size = function(nr, nc){
            "Sets up plotter's buffer size (number of rows and columns)"

            .self$buffer <- array(list(), c(nr, nc))
        },

        set.palette = function(argument, value){
            "Sets the printer's colour palettes"

            if (argument == "all") {
                # recursive call
                nm <- names(.self$palettes)
                if (value[1] == "default") {
                    lapply(seq(along = nm), function(i) {
                        .self$set.palette(nm[i], .self$default.palettes[[i]])
                    })
                } else {
                    lapply(seq(along = nm), function(i) {
                        .self$set.palette(nm[i], value)
                    })
                }
            } else {
                pos <- which(names(.self$palettes) == argument)
                if (length(pos) == 0) {
                    stop("Palette not found: ", argument)
                } else if (value[1] == "default") {
                    .self$palettes[[pos]] <- .self$default.palettes[[pos]]
                } else {
                    .self$palettes[[pos]] <- value
                }
            }
        },

        set.in.buffer = function(myplot, xpos, ypos){
            "Places a plot in the buffer"

            dbuf <- dim(.self$buffer)
            if ( dbuf[1] < xpos | dbuf[2] < ypos) {
                .self$set.buffer.size(xpos, ypos)
            }
            .self$buffer[xpos,ypos] <- list(myplot)
        },

        get.palette = function(type){
            if (type == "all") type <- names(.self$palettes)
            out <- mapply(1:length(type), FUN = function(i) {
                pos <- which(names(.self$palettes) == type[i])
                if (length(pos) == 0) {
                    return(type[i])
                } else {
                    print(paste0(type[i]," = c('",
                                 paste0(.self$palettes[[pos]],
                                        collapse = "', '") ,"')"
                    ), quote = FALSE)
                    return("found")
                }
            })
            if (any(out != "found")) {
                stop("Palette(s) not found: ", out[out != "found"])
            }
        },

        get.buffer.plot = function(){
            "Prints the plots in the buffer"

            vplayout <- function(x, y){
                viewport(layout.pos.row = x, layout.pos.col = y)
            }
            nr <- nrow(.self$buffer)
            nc <- ncol(.self$buffer)
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(nr, nc)))
            for (ii in 1:nr) for (jj in 1:nc) {
                myplot <- .self$buffer[ii, jj][[1]]
                if (!is.null(myplot)) {
                    print(myplot, vp = vplayout(ii, jj))
                }
            }
        }
    )
)
