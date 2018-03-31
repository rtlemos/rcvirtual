#' Modify template configurations via a Shiny app
#'
#' @field shiny.app ANY. Shiny app object.
#' @field uconf rcvirtual.conf. User-defined configuration for daemon,
#' parameters, strategy, and plotter
#' @field previous.txt character. Previously edited value.
#'
#' @import shiny
#' @exportClass rcvirtual.guiconf
#'
setRefClass(
  Class = 'rcvirtual.guiconf',
  contains = c("rcvirtual.basic", "VIRTUAL"),
  fields = list(shiny.app = 'ANY',
                uconf = 'rcvirtual.conf',
                previous.txt = 'character'),
  methods = list(

    initialize = function(uconf){

      if(missing(uconf)) stop('configuration object must be provided')
      stopifnot(is(uconf, 'rcvirtual.conf'))
      .self$previous.txt <- 'Enter text here'
      .self$uconf <- uconf
      .self$shiny.app <- shiny::shinyApp(ui = .self$get.ui(),
                                         server = .self$get.server())
    },

    shiny = function(){
      shiny::runApp(appDir = .self$shiny.app)
      return(.self$uconf)
    },

    set.parameters = function(input, return.obj = TRUE) {
      if (.self$previous.txt != input$text) {
        i <- which(.self$uconf$parameters$name == input$param.name)
        j <- which(names(.self$uconf$parameters) == input$property)
        .self$uconf$parameters[i, j] <- switch(
          class(.self$uconf$parameters[, j]),
          'numeric' = as.numeric(input$text),
          'character' = input$text,
          'logical' = as.logical(input$text)
        )
        .self$previous.txt <- input$text
      }
      if (return.obj) {
        return(.self$uconf$parameters)
      }
    },

    get.server = function() {
      function(input, output, session) {
        observe({
          updateSelectizeInput(
            session, "param.name",
            choices = .self$uconf$parameters$name, server = TRUE)
          updateSelectizeInput(
            session, "property",
            choices = names(.self$uconf$parameters), server = TRUE)
          if(input$exit.button > 0) stopApp(.self$uconf$parameters)
        })
        gui.parameters <- reactive({.self$set.parameters(input)})
        output$show.table <- renderDataTable(gui.parameters())
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
