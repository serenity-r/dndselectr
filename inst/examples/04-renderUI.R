`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(6,
             dragZone("dragzone",
                      choices = list(one = "One",
                                     two = "Two",
                                     three = "Three",
                                     four = "Four"))
      ),
      column(6,
             selectInput("whichzone",
                         label = "Which Zone?",
                         choices = list(stevezone = "Steve", fredzone = "Fred"),
                         selected = "Steve"),
             uiOutput("pickedzone")
      )
    ),
    fluidRow(
      verbatimTextOutput("showme")
    )
  ),
  server = function(input, output) {
    output$showme <- renderText({
      paste(paste("Steve: ", paste(input[["Steve"]], collapse=" ")),
            paste("Fred: ", paste(input[["Fred"]], collapse=" ")),
            sep = "\n")
      })

    output$pickedzone <- renderUI({
      dropZoneInput(input$whichzone %||% "stevezone",
                    choices = list(one = "1",
                                   two = "2",
                                   three = "3",
                                   four = "4"),
                    presets = input[[input$whichzone %||% "stevezone"]])
    })
  }
)

