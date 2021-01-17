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
             h3("Selectable Value"),
             verbatimTextOutput("selectable"),
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
                           presets = list(values = "two",
                                          freeze = "two",
                                          locked = "two",
                                          selected = "two"),
                           multivalued=TRUE,
                           selectable=TRUE,
                           direction="horizontal")
      )
    )
  ),
  server = function(input, output) {
    output$showme <- renderText({ input$dropzone })
    output$selectable <- renderText({ input$dropzone_selected })
    output$locked <- renderText({ input$dropzone_locked })
  }
)

