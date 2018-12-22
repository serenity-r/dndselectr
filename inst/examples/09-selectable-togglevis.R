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
    ),
    fluidRow(
      h3("Visible values"),
      verbatimTextOutput("showme"),
      h3("Invisible values"),
      verbatimTextOutput("invisible"),
      h3("Selected"),
      verbatimTextOutput("selected")
    )
  ),
  server = function(input, output) {
    output$showme <- renderPrint({ input$dropzone })
    output$invisible <- renderPrint({ input$dropzone_invisible })
    output$selected <- renderPrint({ input$dropzone_selected })
  }
)

