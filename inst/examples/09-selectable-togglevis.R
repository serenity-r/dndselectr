library(shiny)
library(dndselectr)

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
             h3("Visible values"),
             verbatimTextOutput("showme"),
             h3("Invisible values"),
             verbatimTextOutput("invisible"),
             h3("Selected"),
             verbatimTextOutput("selected")
      ),
      column(6,
             h3("Dropzone"),
             dropZoneInput("dropzone",
                           choices = list(one = "1",
                                          two = "2",
                                          three = "3",
                                          four = "4"),
                           presets = c("one"),
                           multivalued = TRUE,
                           selectable = TRUE,
                           togglevis = TRUE)
      )
    )
  ),
  server = function(input, output) {
    output$showme <- renderPrint({ paste(setdiff(input$dropzone, input$dropzone_invisible), collapse = " ") })
    output$invisible <- renderPrint({ input$dropzone_invisible })
    output$selected <- renderPrint({ input$dropzone_selected })
  }
)

