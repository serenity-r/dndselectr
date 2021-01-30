library(shiny)
library(reactlog)
library(dndselectr)

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(6,
             h3("Dragzone"),
             dragZone("dragzone", choices = list(one = "One",
                                                 two = "Two",
                                                 three = "Three",
                                                 four = "Four")),
             h3("Dropzone Values"),
             verbatimTextOutput("hidden")
      ),
      column(6,
             h2("Entangled Dropzones"),
             h3("Dropzone (Hidden Items)"),
             dropZoneInput("dropzone_hidden", choices = list(one = "1",
                                                             two = "2",
                                                             three = "3",
                                                             four = "4"),
                           hidden = TRUE,
                           placeholder = "Drop items here!",
                           highlight = TRUE,
                           presets = list(values = "one")),
             h3("Dropzone (Visible Items)"),
             dropZoneInput("dropzone_visible", choices = list(one = "1",
                                                              two = "2",
                                                              three = "3",
                                                              four = "4"),
                           presets = list(values = "one"))
      )
    ),
    reactlog_module_ui()
  ),
  server = function(input, output, session) {
    output$hidden <- renderText({
      paste(paste("Hidden: ", paste(input$dropzone_hidden, collapse=" ")),
            paste("Visible: ", paste(input$dropzone_visible, collapse=" ")),
            sep = "\n")
      })

    entangleInputs(session, "dropzone_visible", "dropzone_hidden")

    reactlog_module_server()
  }
)
