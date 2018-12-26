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
             h3("Dropzone"),
             dropZoneInput("dropzone",
                           choices = list(one = "1",
                                          two = "2",
                                          three = "3",
                                          four = "4"),
                           presets = c("one"),
                           multivalued = TRUE,
                           selectable = TRUE)
      )
    )
  ),
  server = function(input, output) {
    output$showme <- renderPrint({ input$dropzone })
    output$selected <- renderPrint({ input$dropzone_selected })
  }
)

