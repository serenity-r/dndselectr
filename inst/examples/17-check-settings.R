library(shiny)
library(dndselectr)

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
             fluidRow(
               column(6,
                      h3("Dropzone Settings"),
                      verbatimTextOutput("settings"),
                      textInput("new_placeholder", label="Dropzone Placeholder"),
                      actionButton("update_placeholder", "Update Dropzone Placeholder"),
                      checkboxGroupInput("new_presets", label="Presets",
                                         choices = c("one", "two", "three", "four"),
                                         selected = c("one", "three"),
                                         inline = TRUE),
                      checkboxGroupInput("new_locked", label="Locked",
                                         choices = c("one", "two", "three", "four"),
                                         selected = c("one", "three"),
                                         inline = TRUE),
                      radioButtons("new_selected", label="Selected",
                                   choices = c("one", "two", "three", "four"),
                                   selected = c("one"),
                                   inline = TRUE),
                      checkboxGroupInput("new_invisible", label="Invisible",
                                         choices = c("one", "two", "three", "four"),
                                         selected = c("one", "three"),
                                         inline = TRUE),
                      actionButton("update_presets", "Update Dropzone Presets"),
                      checkboxGroupInput("new_drop_choices", label="Choices",
                                         choiceNames = list("1", "2", "3", "4"),
                                         choiceValues = list("one", "two", "three", "four"),
                                         selected = c("one", "two", "three", "four"),
                                         inline = TRUE),
                      actionButton("update_drop_choices", "Update Dropzone Choices")
               ),
               column(6,
                      h3("Dragzone Settings"),
                      checkboxGroupInput("new_drag_choices", label="Choices",
                                         choices = c("one", "two", "three", "four"),
                                         selected = c("one", "three"),
                                         inline = TRUE),
                      actionButton("update_drag_choices", "Update Dragzone choices")
               )
             )
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
                           togglelock = TRUE,
                           replaceOnDrop = TRUE)
      )
    )
  ),
  server = function(input, output, session) {
    output$showme <- renderPrint({ input$dropzone })
    output$settings <- renderPrint({
      c(input$dropzone_settings,
        input$dropzone_locked)
    })

    observeEvent(input$update_placeholder, {
      updateDropZoneInput(session, "dropzone", placeholder = input$new_placeholder)
    })

    observeEvent(input$update_presets, {
      updateDropZoneInput(session, "dropzone", presets = list(values = input$new_presets %||% NA,
                                                              locked = input$new_locked %||% NA,
                                                              selected = input$new_selected %||% NA,
                                                              invisible = input$new_invisible %||% NA))
    })

    observeEvent(input$update_drop_choices, {
      updateDropZoneInput(session, "dropzone", choices = list(one = "1",
                                                              two = "2",
                                                              three = "3",
                                                              four = "4")[input$new_drop_choices])
    })

    observeEvent(input$update_drag_choices, {
      updateDragZone(session, "dragzone", choices = input$new_drag_choices)
    })
  }
)

