`%||%` <- function(a, b) if (is.null(a)) b else a

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
             h3("Dropzone Settings"),
             verbatimTextOutput("settings"),
             textInput("new_placeholder", label="Dropzone Placeholder"),
             actionButton("update_placeholder", "Update Dropzone Placeholder"),
             checkboxGroupInput("new_presets", label="Presets",
                                choices = c("one", "two", "three", "four"),
                                selected = c("one", "three"),
                                inline = TRUE),
             actionButton("update_presets", "Update Dropzone Presets")
      ),
      column(6,
             h3("Dropzone"),
             dropZoneInput("dropzone",
                           choices = list(one = "1",
                                          two = "2",
                                          three = "3",
                                          four = "4"),
                           presets = c("one", "three"),
                           multivalued = FALSE,
                           replaceOnDrop = TRUE)
      )
    )
  ),
  server = function(input, output, session) {
    output$showme <- renderPrint({ input$dropzone })
    output$settings <- renderPrint({
      input$dropzone_settings
    })

    observeEvent(input$update_placeholder, {
      updateDropZoneInput(session, "dropzone", placeholder = input$new_placeholder)
    })

    observeEvent(input$update_presets, {
      updateDropZoneInput(session, "dropzone", presets = input$new_presets %||% NA)
    })
  }
)

