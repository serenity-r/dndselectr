`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(6,
             h3("Dragzone"),
             dragZone("dragzone",
                      choices = list(one = "One",
                                     two = "Two",
                                     three = "Three",
                                     four = "Four")),
             h3("Dropzone Values"),
             verbatimTextOutput("showme"),
             h3("Selected Values"),
             verbatimTextOutput("selected")
      ),
      column(6,
             h3("Dropzones"),
             selectInput("whichzone",
                         label = "Which Zone?",
                         choices = c(Steve = "stevezone", Fred = "fredzone"),
                         selected = "Steve"),
             uiOutput("pickedzone")
      )
    )
  ),
  server = function(input, output) {
    output$showme <- renderText({
      paste(paste("Steve: ", paste(input$stevezone, collapse=" ")),
            paste("Fred: ", paste(input$fredzone, collapse=" ")),
            sep = "\n")
    })

    output$selected <- renderText({
      paste(paste("Steve: ", paste(input$stevezone_selected, collapse=" ")),
            paste("Fred: ", paste(input$fredzone_selected, collapse=" ")),
            sep = "\n")
    })

    output$pickedzone <- renderUI({
      dropZoneInput(input$whichzone %||% "stevezone",
                    choices = list(one = "1",
                                   two = "2",
                                   three = "3",
                                   four = "4"),
                    multivalued = TRUE,
                    selectable = TRUE,
                    selectOnDrop = TRUE,
                    presets = list(values = input[[input$whichzone %||% "stevezone"]],
                                   selected = input[[paste0(input$whichzone, '_selected') %||% NULL]]))
    })
  }
)

