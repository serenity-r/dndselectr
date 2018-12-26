shinyApp(
  ui = fluidPage(
    fluidRow(
      column(6,
             h3("Dragzone"),
             dragZone("dragzone", choices = list(one = "One",
                                                 two = "Two",
                                                 three = "Three",
                                                 four = "Four")),
             h3("Values"),
             verbatimTextOutput("showme"),
             h3("Multivalues"),
             verbatimTextOutput("showmeMultivalues")
      ),
      column(6,
             h3("Dropzone"),
             dropZoneInput("dropzone", choices = list(one = "1",
                                                      two = "2",
                                                      three = "3",
                                                      four = "4"),
                           multivalued = TRUE,
                           presets = c("one", "two"))
      )
    )
  ),
  server = function(input, output) {
    output$showme <- renderPrint({ input$dropzone })
    output$showmeMultivalues <- renderPrint({ multivalues( input$dropzone ) })
  }
)

