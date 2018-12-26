shinyApp(
  ui = fluidPage(
    fluidRow(
      column(6,
             dragZone("dragzone", choices = list(one = "One",
                                                 two = "Two",
                                                 three = "Three",
                                                 four = "Four"))
      ),
      column(6,
             dropZoneInput("dropzone_hidden", choices = list(one = "1",
                                                             two = "2",
                                                             three = "3",
                                                             four = "4"),
                           hidden = TRUE,
                           placeholder = "Drop items here!",
                           highlight = TRUE,
                           presets = list(values = "one",
                                          locked = "one")),
             dropZoneInput("dropzone_visible", choices = list(one = "1",
                                                              two = "2",
                                                              three = "3",
                                                              four = "4"),
                           presets = list(values = "one",
                                          locked = "one"))
      )
    ),
    fluidRow(
      h3("Hidden"),
      verbatimTextOutput("showme_hidden"),
      h3("Visible"),
      verbatimTextOutput("showme_visible")
    )
  ),
  server = function(input, output, session) {
    output$showme_hidden <- renderPrint({ input$dropzone_hidden })
    output$showme_visible <- renderPrint({ input$dropzone_visible })

    entangle(session, "dropzone_hidden", "dropzone_visible")
  }
)
