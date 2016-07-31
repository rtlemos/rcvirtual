#' Reference class controller of a Shiny app
#'
#' @field df data.frame.
#' @field previous.txt character. Previously edited value.
#'
#' @import shiny
#' @exportClass rcvirtual.gui
#'
setRefClass(
  Class = 'rcvirtual.gui',
  contains = c("rcvirtual.basic", "VIRTUAL"),
  fields = list(df = 'data.frame',
                previous.txt = 'character'),
  methods = list(

    initialize = function(df = NULL) {
      .self$previous.txt <- 'Enter text here'
      if (is.null(df)) {
        gui.default.conf <- setRefClass(Class = 'gui.default.conf',
                                        contains = c('rcvirtual.conf'))
        gconf <- gui.default.conf()
        .self$df <- gconf$get.conf.template()
      } else {
        .self$df <- df
      }
      .self$start()
    },

    set.df = function(input, return.obj = TRUE) {
      if (.self$previous.txt != input$text) {
        i <- which(.self$df$name == input$param.name)
        j <- which(names(.self$df) == input$property)
        if (is.numeric(.self$df[, j])) {
          .self$df[i, j] <- as.numeric(input$text)
        } else if(is.character(.self$df[, j])) {
          .self$df[i, j] <- input$text
        } else {
          cat(class(.self[,j]))
          stop('df column is neither numeric nor character.')
        }
        .self$previous.txt <- input$text
      }
      if (return.obj) {
        return(.self$df)
      }
    },

    start = function() {

      server <- function(input, output, session) {

        observe({
          updateSelectizeInput(session, "param.name",
                               choices = .self$df$name, server = TRUE)
          updateSelectizeInput(session, "property",
                               choices = names(.self$df), server = TRUE)
        })


        gui.df <- reactive({.self$set.df(input)})
        output$show.table <- renderDataTable(gui.df())
      }

      ui <- navbarPage(
        title = 'RC GUI',
        tabPanel(
          'Parameter table',
          sidebarLayout(
            sidebarPanel(
              width = 2,
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
              textInput("text", label = h5("Property value"),
                        value = "Enter text here")
            ),
            mainPanel(
              fluidRow(column(12, dataTableOutput('show.table')))
            )
          )
        )
      )
      shinyApp(ui = ui, server = server)
    }
  )
)
