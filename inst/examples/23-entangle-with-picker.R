library(shiny)
library(reactlog)
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
             verbatimTextOutput("showme")
      ),
      column(6,
             h3("Dropzone"),
             shinyWidgets::pickerInput(
               inputId = "dropzone_picker",
               label = NULL,
               selected = "one",
               choices = list("1" = "one",
                              "2" = "two",
                              "3" = "three",
                              "4" = "four"),
               options = list(
                 title = "Nothing selected",
                 size = 6,
                 `live-search` = FALSE,
                 `dropup-auto` = FALSE
               )
             ),
             dropZoneInput("dropzone",
                           choices = list(one = "1",
                                          two = "2",
                                          three = "3",
                                          four = "4"),
                           presets = c("one"),
                           chooser = FALSE)
      )
    ),
    reactlog_module_ui()
  ),
  server = function(input, output, session) {
    entangleInputs(session, dropzone = "DropZone", dropzone_picker = "Picker")

    output$showme <- renderPrint({ input$dropzone })

    reactlog_module_server()
  }
)

