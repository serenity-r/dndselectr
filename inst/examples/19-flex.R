shinyApp(
  ui = fluidPage(
    tags$style(HTML(".ds-dropoption, .ds-dragitem.gu-transit { width: 400px !important; }
                     #vzone { height: 100px; }")),
    fluidRow(
      column(6,
             h3("Dragzone"),
             dragZone("dragzone",
                      choices = list(one = "One",
                                     two = "Two",
                                     three = "Three",
                                     four = "Four")),
             h3("Dropzone Values"),
             verbatimTextOutput("showme")
      ),
      column(6,
             h3("Dropzone: Horizontal"),
             dropZoneInput("hzone",
                           choices = list(one = "1",
                                          two = "2",
                                          three = "3",
                                          four = "4"),
                           direction="horizontal",
                           multivalued=TRUE,
                           selectable=TRUE,
                           flex=TRUE),
             h3("Dropzone: Vertical"),
             dropZoneInput("vzone",
                           choices = list(one = "1",
                                          two = "2",
                                          three = "3",
                                          four = "4"),
                           direction="vertical",
                           multivalued=TRUE,
                           selectable=TRUE,
                           flex=TRUE)
      )
    )
  ),
  server = function(input, output) {
    output$showme <- renderText({
      paste(paste("Dropzone A:", paste0(input$hzone, collapse = " ")),
            paste("Dropzone B:", paste0(input$vzone, collapse = " ")), sep="\n")
    })
  }
)

