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
             h3("Dropzone Values"),
             verbatimTextOutput("showme"),
             h3("Locked Values"),
             verbatimTextOutput("locked")
      ),
      column(6,
             h3("Dropzone"),
             dropZoneInput("dropzone",
                           choices = list(one = "1",
                                          two = "2",
                                          three = "3",
                                          four = "4"),
                           presets = c("one"),
                           togglelock = TRUE)
      )
    )
  ),
  server = function(input, output) {
    output$showme <- renderPrint({ input$dropzone })
    output$locked <- renderPrint({ input$dropzone_locked })
  }
)

