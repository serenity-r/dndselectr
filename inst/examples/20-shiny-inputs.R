`%||%` <- function(a, b) if (is.null(a)) b else a

createItem <- function(id, server=FALSE, session=getDefaultReactiveDomain()) {
  ns <- NS(id)

  item <- paste("Slider", id)
  if (server) {
    item <- sliderInput(ns('slider'), id, 0, 1, session$input[[ns('slider')]] %||% 1)
  }

  tagList(
    HTML("<style>
         .ds-dropzone .ds-dropoption { cursor: default; }
         .ds-handle { cursor: move; }
         </style>"),
    div(class = "blue",
        icon("bars", class="ds-handle"),
        item
        )
  )
}

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(6,
             h3("Dragzone"),
             dragZone("dragzone",
                      choices = list(one = "One",
                                     two = "Two")),
             h3("Dropzone Values"),
             verbatimTextOutput("showme"),
             textInput("item", label = h3("Item to append or select"), value = ""),
             actionButton("append", label = "Append to dropzone"),
             actionButton("select", label = "Select"),
             actionButton("remove", label = "Remove selected"),
             checkboxGroupInput("new_presets", label="Presets",
                                choices = c("one", "two"),
                                selected = c("one"),
                                inline = TRUE),
             actionButton("update_presets", "Update Dropzone Presets")
      ),
      column(6,
             h3("Dropzone"),
             dropZoneInput("dropzone",
                           choices = list(one = createItem("one"),
                                          two = createItem("two")),
                           presets = c("one"),
                           selectable = TRUE,
                           server = createItem),
             verbatimTextOutput("one")
      )
    )
  ),
  server = function(input, output, session) {
    dropZoneServer(session, "dropzone", createItem)

    output$showme <- renderText({
      paste("Dropzone:", paste0(input$dropzone, collapse = " "))
    })

    output$one <- renderPrint({
      reactiveValuesToList(input)
    })

    observeEvent(input$append, {
      dndselectr::appendToDropzone(session, input$item, "dropzone")
    })

    observeEvent(input$select, {
      dndselectr::select(session, input$item, "dropzone")
    })

    observeEvent(input$remove, {
      dndselectr::removeSelected(session, "dropzone")
    })

    observeEvent(input$update_presets, {
      dndselectr::updateDropZoneInput(session, "dropzone", presets = input$new_presets %||% NA)
    })
  }
)

