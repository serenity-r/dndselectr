shinyApp(
  ui = fluidPage(
    tags$head(
      tags$style(HTML("
        #hzone .ds-dropoption,
        #hzone .gu-transit {
          vertical-align: middle;
          display: inline-block;
          text-align: center;
          width: 30px;
          height: 30px;
          margin: 3px 0 3px 3px;
        }"))
      ),
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
             h3("Horizontal"),
             dropZoneInput("hzone",
                           choices = list(one = "1",
                                          two = "2",
                                          three = "3",
                                          four = "4"),
                           direction="horizontal"),
             h3("Vertical"),
             dropZoneInput("vzone",
                           choices = list(one = "1",
                                          two = "2",
                                          three = "3",
                                          four = "4"),
                           direction="vertical")
      )
    ),
    fluidRow(
      h3("Values"),
      verbatimTextOutput("showme")
    )
  ),
  server = function(input, output) {
    output$showme <- renderText({
      paste(paste("Dropzone A:", paste0(input$hzone, collapse = " ")),
            paste("Dropzone B:", paste0(input$vzone, collapse = " ")), sep="\n")
    })
  }
)

