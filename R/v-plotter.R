#' Graphics controller (plotter) of statistical models
#'
#' This is a virtual Reference Class for plotter RCs
#'
#' This RC contains fields (a.k.a. "attributes") and methods
#' (a.k.a. "procedures") for that any plotter RC must have.
#'
#' @field shiny.app ANY. Shiny app to visualize model
#'
#' @import grid
#' @importFrom methods new
#' @exportClass rcvirtual.plotter
#'
setRefClass(
  Class = "rcvirtual.plotter",
  contains = c("rcvirtual.basic", "VIRTUAL"),
  fields = list(
    buffer = "matrix",
    palettes = "list",
    default.palettes = "list",
    shiny.app = 'ANY',
    parameters = "rcvirtual.parameters",
    strategy = "rcvirtual.strategy"),
  methods = list(

    # ------------------------------------------------------
    # Initializer methods ----------------------------------
    # ------------------------------------------------------

    construct = function(){
      "Constructs the printer object"

      callSuper()

      #default palettes are colour-blind friendly
      dfp <- c("#000000", "#E69F00", "#56B4E9",
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")
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
        neg.zero.pos =
          .self$default.palettes$neg.zero.pos,
        zero.pos = .self$default.palettes$zero.pos

      )
      .self$shiny.app <- shinyApp(ui = .self$get.ui(),
                                  server = .self$get.server())
    },

    construct.layer = function(type = NULL, df = NULL,
                               pch = NULL, cex = NULL,
                               xlim = NULL, ylim = NULL,
                               zlim = NULL){
      "Generates a layer (list) for plotting, based on a
      data frame and additional specs"

      ly <- list(type = NA, df = NA, pch = NA, cex = NA,
                 xlim = NA, ylim = NA, zlim = NA)

      if (class(type)[1] == "character") ly$type <- type
      if (class(pch)[1] == "numeric") ly$pch <- pch
      if (class(cex)[1] == "numeric") ly$cex <- cex
      if (class(xlim)[1] == "numeric") ly$xlim <- xlim
      if (class(ylim)[1] == "numeric") ly$ylim <- ylim
      if (class(zlim)[1] == "numeric") ly$zlim <- zlim
      if (class(df)[1] == "data.frame") {
        ly$df <- df
        if (class(xlim)[1] == "NULL" &
            class(df[,1])[1] == "numeric") {
          ly$xlim <- c(min(df[, 1]), max(df[, 1]))
        }
        if (class(ylim)[1] == "NULL" &
            class(df[,2])[1] == "numeric") {
          ly$ylim <- c(min(df[, 2]), max(df[, 2]))
        }
        if (class(zlim)[1] == "NULL" & ncol(df) > 2) {
          if (class(df[,3])[1] == "numeric") {
            ly$zlim <- c(min(df[, 3]), max(df[, 3]))
          }
        }
      }
      return(ly)
    },

    shiny = function() {
      runApp(appDir = .self$shiny.app)
    },

    # ------------------------------------------------------
    # Set methods ------------------------------------------
    # ------------------------------------------------------

    set.buffer.size = function(nr = 1, nc = 1){
      "Sets up plotter's buffer size
      (number of rows and columns)"

      .self$buffer <- array(list(), c(nr, nc))
    },

    set.palette = function(argument, value){
      "Sets the printer's colour palettes"

      if (argument == "all") {
        # recursive call
        nm <- names(.self$palettes)
        if (value[1] == "default") {
          lapply(seq(along = nm), function(i) {
            .self$set.palette(
              nm[i],
              .self$default.palettes[[i]])
          })
        } else {
          lapply(seq(along = nm), function(i) {
            .self$set.palette(nm[i], value)
          })
        }
      } else {
        pos <- which(
          names(.self$palettes) == argument)
        if (length(pos) == 0) {
          stop("Palette not found: ", argument)
        } else if (value[1] == "default") {
          .self$palettes[[pos]] <-
            .self$default.palettes[[pos]]
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

    # ------------------------------------------------------
    # Get methods ------------------------------------------
    # ------------------------------------------------------

    get.palette = function(type){
      if (type == "all") type <- names(.self$palettes)
      out <- mapply(1:length(type), FUN = function(i){
        pos <- which(
          names(.self$palettes) == type[i])
        if (length(pos) == 0) {
          return(type[i])
        } else {
          q1 <- paste0(.self$palettes[[pos]], collapse = "', '")
          q2 <- paste0(type[i]," = c('", q1, "')")
          print(q2, quote = FALSE)
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
        grid::viewport(layout.pos.row = x,
                       layout.pos.col = y)
      }
      nr <- nrow(.self$buffer)
      nc <- ncol(.self$buffer)
      grid::grid.newpage()
      grid::pushViewport(
        grid::viewport(
          layout = grid::grid.layout(nr, nc))
      )
      for (ii in 1:nr) for (jj in 1:nc) {
        myplot <- .self$buffer[ii, jj][[1]]
        if (!is.null(myplot)) {
          print(myplot, vp = vplayout(ii, jj))
        }
      }
    },

    get.domain.plot = function(){
      "Plots the region of interest"

      mystates <- .self$get.state.layer(
        specs = list(
          latb = .self$parameters$get.value(
            long.name = "latitude"),
          lonb = .self$parameters$get.value(
            long.name = "latitude")))
      specs.plot <- list(under.data = "state",
                         main.data = mystates$names,
                         top.data = mystates$rect,
                         under.specs = list(type = "map"),
                         main.specs = list(type = "text"),
                         top.specs = NULL,
                         xyrange = "main",
                         labels = c("Domain",
                                    "Longitude",
                                    "Latitude", NA))
      .self$get.plot.surface(specs = specs.plot)
    },

    get.detail.plot = function(input = NULL){
      "TODO: add descriptor"

      if (is.null(input)) {
        input <- list(udata = "state")
      }
      specs.plot <- list(
        under.data = NULL,
        main.data = "elevation",
        top.data = list(
          "elevation",
          data.frame(lon = -122.45, lat = 37.6, z = 0)),
        under.specs = NULL,
        main.specs = list(
          type = "raster", include.z = c(0, Inf)),
        top.specs = list(
          list(type = "contour"),
          list(type = "points", pch = 14, cex = 1)),
        xyrange = "main",
        labels = c("Plot of elevation", "Longitude",
                   "Latitude", "Elevation"))
      .self$get.plot.surface(specs = specs.plot)
    },

    get.state.layer = function(specs){
      "Uses state_coords.RData, loaded with package, to plot
      USA states according to the specs provided"

      my.state <- mapply(1:2, FUN = function(i){
        which.min(
          (specs$latb[i] - state.coords$latitude) ^ 2 +
            (specs$lonb[i] - state.coords$longitude) ^ 2)
      })
      mylatb <- c(min(state.coords$llat[my.state[1]],
                      state.coords$llat[my.state[2]]),
                  max(state.coords$ulat[my.state[1]],
                      state.coords$ulat[my.state[2]]))
      mylonb <- c(min(state.coords$llon[my.state[1]],
                      state.coords$llon[my.state[2]]),
                  max(state.coords$ulon[my.state[1]],
                      state.coords$ulon[my.state[2]]))
      tol <- min(0.5, max(abs(mylonb[2] - mylonb[1]),
                          abs(mylatb[2] - mylatb[1])) / 2.0)
      xlim <- c(mylonb[1] - tol, mylonb[2] + tol)
      ylim <- c(mylatb[1] - tol, mylatb[2] + tol)

      crit <- (state.coords$longitude >= mylonb[1] &
                 state.coords$longitude <= mylonb[2] &
                 state.coords$latitude >= mylatb[1] &
                 state.coords$latitude <= mylatb[2])
      statelist <- list(
        names = .self$construct.layer(
          type = "text",
          df = data.frame(x = state.coords$longitude[crit],
                          y = state.coords$latitude[crit],
                          z = state.coords$code[crit]),
          xlim = xlim,
          ylim = ylim),
        rect = .self$construct.layer(
          type = "rect",
          df = data.frame(x = specs$lonb, y = specs$latb),
          xlim = xlim,
          ylim = ylim)
      )
      return(statelist)
    },

    get.layer = function(mydata, myspecs){
      "Converts a chunk of data into a list for plotting"

      io.names <- .self$parameters$get.names(long = TRUE)
      ic <- class(mydata)[1]
      m <- switch(
        ic,
        "NULL" = NULL, #return NULL
        "list" = mydata, #return = input
        "character" = {
          my.id <- which(io.names == mydata)
          if (length(my.id) == 1) {
            dt <- .self$parameters$get.data(
              long.name = mydata)
            mylayer <- switch(
              myspecs$type,
              "contour" = ,
              "raster" = .self$construct.layer(
                type = myspecs$type,
                df = .self$parameters$netcdf.io[[my.id]]$get.xyz(
                  z.name = mydata,
                  include.z = myspecs$include.z,
                  bounds = list(
                    lon = .self$parameters$get.lon.bounds(),
                    lat = .self$parameters$get.lat.bounds(),
                    time = .self$parameters$get.time.bounds()
                  )
                )),
              NULL
            )
          } else if (mydata == "usa" |
                     mydata == "state" |
                     mydata == "county") {
            mylayer <- .self$construct.layer(type = mydata)
          } else {
            dt <- .self$parameters$get.value(mydata)
            if (any(names(dt) == "longitude")) {
              myz <- as.numeric(t(dt[[3]]))
              myl <- expand.grid(lon = dt$longitude,
                                 lat = rev(dt$latitude))
              #using myz = 1 as mask
              uz <- unique(myz)
              if (length(uz) == 2 & any(uz == 1)) {
                crit <- (myz == 1)
              } else {
                crit <- rep(TRUE, length(myz))
              }
              mylayer <- switch(
                myspecs$type,
                "points" = list(
                  type = myspecs$type,
                  df = data.frame(x = myl$lon[crit],
                                  y = myl$lat[crit]),
                  pch = myspecs$pch,
                  cex = myspecs$cex),
                "line" = ,
                "path" = ,
                "contour" = ,
                "raster" = list(
                  type = myspecs$type,
                  df = data.frame(x = myl$lon[crit],
                                  y = myl$lat[crit],
                                  z = myz[crit])),
                NULL
              )
            } else {
              mylayer <- .self$get.special.layer(
                id.char = mydata, lspecs = myspecs)
            }
          }
          mylayer #return what was constructed
        },
        "data.frame" = .self$construct.layer(
          type = myspecs$type,
          df = data.frame(x = mydata$lon,
                          y = mydata$lat,
                          z = mydata$z),
          pch = myspecs$pch,
          cex = myspecs$cex),
        default = NULL
      )
      return(m)
    },

    get.plot.surface = function(specs, xpos = 1, ypos = 1){
      "Plots data, e.g.
      specs <- list(main.data = 'elevation', do.grid = TRUE,
      under.data = 'elevation')
      "

      # Under layer
      under <- .self$get.layer(mydata = specs$under.data,
                               myspecs = specs$under.specs)

      # Main layer
      main <- .self$get.layer(mydata = specs$main.data,
                              myspecs = specs$main.specs)

      #Top layer
      if (class(specs$top.data)[1] == "plot.layer") {
        top <- list()
        top[[1]] <- specs$top.data
      } else {
        top <- lapply(seq_along(specs$top.data),
                      FUN = function(i){
                        .self$get.layer(
                          mydata = specs$top.data[[i]],
                          myspecs = specs$top.specs[[i]])
                      })
      }

      # Base layer
      if (all(is.null(specs$xyrange))) {
        base.template <- "main"
      } else {
        base.template <- specs$xyrange
      }
      if (class(base.template)[1] == "character") {
        base <- list(labels = specs$labels,
                     xlim = (get(base.template))$xlim,
                     ylim = (get(base.template))$ylim,
                     zlim = specs$zlim)
      } else {
        base <- list(labels = specs$labels,
                     xlim = base.template$xlim,
                     ylim = base.template$ylim,
                     zlim = specs$zlim)
      }

      #Plotting
      .self$surfaceplot2(base = base, under = under,
                         main = main, top = top,
                         xpos = xpos, ypos = ypos)
    },

    get.server = function() {
      function(input, output, session) {
        observe({
          updateSelectizeInput(
            session, "highlight.node.name",
            choices = .self$strategy$graph$names, server = TRUE)
          if(input$exit.button > 0) stopApp()
        })
        output$dag <- renderPlot({.self$graphplot(
          .self$strategy$graph,
          input$highlight.node.name,
          input$highlight.edges,
          col = c('black',
                  ifelse(input$hide.fixed, 'white', 'azure2'),
                  'firebrick2'),
          do.plot = TRUE)
        })
      }
    },

    get.ui = function() {
      navbarPage(
        title = 'RC Plotter',
        tabPanel(
          'Daemon'
        ),
        tabPanel(
          'Strategy',
          fluidPage(
            fluidRow(
              column(
                2,
                selectizeInput(
                  inputId = "highlight.node.name",
                  label = "Highlight parameter",
                  multiple  = FALSE,
                  choices = NULL
                ),
                selectizeInput(
                  inputId = "highlight.edges",
                  label = "Highlight edges",
                  multiple  = FALSE,
                  choices = c('from', 'to')
                ),
                checkboxInput("hide.fixed",
                              label = "Hide constants", value = FALSE),
                br(),
                actionButton("exit.button", "Exit",
                             icon("paper-plane"),
                             style = paste0("color: #fff; background-color: ",
                                            "#337ab7; border-color: #2e6da4"))
              ),
              column(
                10,
                plotOutput('dag')
              )
            )
          )
        ),
        tabPanel(
          'Plotter'
        ),
        tabPanel(
          'Parameters'
        )
      )
    }

    # ------------------------------------------------------
    # Is methods -------------------------------------------
    # ------------------------------------------------------
  )
)
