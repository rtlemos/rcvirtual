#' Modify template configurations via a Shiny app
#'
#' @field df data.frame.
#' @field previous.txt character. Previously edited value.
#'
#' @import shiny
#' @exportClass rcvirtual.guiconf
#'
setRefClass(
  Class = 'rcvirtual.guiconf',
  contains = c("rcvirtual.basic", "VIRTUAL"),
  fields = list(shiny.app = 'ANY',
                df = 'data.frame',
                previous.txt = 'character'),
  methods = list(

    initialize = function(conf){

      if(missing(conf)) stop('configuration object must be provided')
      .self$previous.txt <- 'Enter text here'
      .self$df <- conf$parameters
      .self$shiny.app <- shinyApp(ui = .self$get.ui(),
                                  server = .self$get.server())
    },

    launch.app = function(){
      runApp(appDir = .self$shiny.app)
      return(.self$df)
    },

    set.df = function(input, return.obj = TRUE) {
      if (.self$previous.txt != input$text) {
        i <- which(.self$df$name == input$param.name)
        j <- which(names(.self$df) == input$property)
        .self$df[i, j] <- switch(
          class(.self$df[, j]),
          'numeric' = as.numeric(input$text),
          'character' = input$text,
          'logical' = as.logical(input$text)
        )
        .self$previous.txt <- input$text
      }
      if (return.obj) {
        return(.self$df)
      }
    },

    get.server = function() {
      function(input, output, session) {
        observe({
          updateSelectizeInput(session, "param.name",
                               choices = .self$df$name, server = TRUE)
          updateSelectizeInput(session, "property",
                               choices = names(.self$df), server = TRUE)
          if(input$exit.button > 0) stopApp(.self$df)
        })
        gui.df <- reactive({.self$set.df(input)})
        output$show.table <- renderDataTable(gui.df())
      }
    },

    get.ui = function() {
      navbarPage(
        title = 'RC GUI Configuration',
        tabPanel(
          'Daemon'
        ),
        tabPanel(
          'Strategy'
        ),
        tabPanel(
          'Plotter'
        ),
        tabPanel(
          'Parameters',
          fluidPage(
            fluidRow(
              column(
                2,
                selectizeInput(
                  inputId = "param.name",
                  label = "Parameter name",
                  multiple  = FALSE,
                  choices = NULL
                ),
                selectizeInput(
                  inputId = "property",
                  label = "Property name",
                  multiple  = FALSE,
                  choices = NULL
                ),
                textInput("text", label = h5(strong("Property value")),
                          value = "Enter text here"),
                br(),
                actionButton("exit.button", "Exit",
                             icon("paper-plane"),
                             style = paste0("color: #fff; background-color: ",
                                            "#337ab7; border-color: #2e6da4"))
              ),
              column(
                10,
                div(style = 'overflow-x: scroll; overflow-y: scroll',
                    dataTableOutput('show.table'))
              )
            )
          )
        )
      )
    }
  )
)
