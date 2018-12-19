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
      h3("Options"),
      verbatimTextOutput("showme"),
      h3("Selected"),
      verbatimTextOutput("selected_steve"),
      verbatimTextOutput("selected_fred")
    )
  ),
  server = function(input, output) {
    output$showme <- renderText({
      paste(paste("Steve: ", paste(input[["Steve"]], collapse=" ")),
            paste("Fred: ", paste(input[["Fred"]], collapse=" ")),
            sep = "\n")
    })

    output$selected_steve <- renderText({
      paste("Steve: ", input$Steve_selected)
    })

    output$selected_fred <- renderText({
      paste("Fred: ", input$Fred_selected)
    })

    output$pickedzone <- renderUI({
      dropZoneInput(input$whichzone %||% "stevezone",
                    choices = list(one = "1",
                                   two = "2",
                                   three = "3",
                                   four = "4"),
                    multivalued = TRUE,
                    selectable = TRUE,
                    presets = input[[input$whichzone %||% "stevezone"]],
                    selected = input[[paste0(input$whichzone, '_selected') %||% NULL]])
    })
  }
)

