# utility function
`%||%` <- function(a, b) if (is.null(a)) b else a

# Module: UI for item
itemUI <- function(id, server=FALSE, session=getDefaultReactiveDomain()) {
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
        item,
        switch(as.character(server),
               "TRUE" = verbatimTextOutput(ns('sliderval')),
               "FALSE" = NULL)
    )
  )
}

# Module: Server for item
itemServer <- function(input, output, session) {
  output$sliderval <- renderPrint({
    input$slider
  })
}

# Main app
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
                           choices = list(one = itemUI("one"),
                                          two = itemUI("two")),
                           presets = c("one"),
                           selectable = TRUE,
                           server = itemUI),
             verbatimTextOutput("inputvals")
      )
    )
  ),
  server = function(input, output, session) {
    # The next three chunks of code handle server UI for dropzone, including
    #   calling server modules on newly created items.

    # (1) Insert server UI code for dropzone
    dropZoneServer(session, "dropzone", itemUI)

    # (2) This stores returned reactives from layer modules
    layer_modules <- reactiveValues()

    # (3) Call server module for new dropzone items
    observeEvent(input$dropzone, {
      # Adding new item
      purrr::map(setdiff(input$dropzone, names(layer_modules)), ~ { layer_modules[[.]] <- callModule(module = itemServer, id = .)} )
      # Remove old item
      purrr::map(setdiff(names(layer_modules), input$dropzone), ~ { layer_modules[[.]] <- NULL })
    }, priority = 1)

    output$showme <- renderText({
      paste("Dropzone:", paste0(input$dropzone, collapse = " "))
    })

    output$inputvals <- renderPrint({
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
