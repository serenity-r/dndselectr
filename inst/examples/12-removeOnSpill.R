shinyApp(
  ui = fluidPage(
    fluidRow(
      column(6,
             h3("Dragzone"),
             dragZone("dragzone",
                      choices = list(one = "One",
                                     two = "Two",
                                     three = "Three",
                                     four = "Four"))
      ),
      column(6,
             h3("Do not remove on drag"),
             dropZoneInput("dropzone",
                           choices = list(one = "1",
                                          two = "2",
                                          three = "3",
                                          four = "4"),
                           presets = c("one"),
                           removeOnSpill=FALSE),
             h3("Remove on drag"),
             dropZoneInput("flopzone",
                           choices = list(one = "1",
                                          two = "2",
                                          three = "3",
                                          four = "4"),
                           presets = c("two"),
                           removeOnSpill=TRUE)
      )
    ),
    fluidRow(
      h3("Values"),
      verbatimTextOutput("showme")
    )
  ),
  server = function(input, output) {
    output$showme <- renderText({
      paste(paste("Dropzone A:", paste0(input$dropzone, collapse = " ")),
            paste("Dropzone B:", paste0(input$flopzone, collapse = " ")), sep="\n")
    })
  }
)

