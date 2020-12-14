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
             h3("Selected"),
             verbatimTextOutput("selected"),
             h3("Invisible"),
             verbatimTextOutput("invisible"),
             h3("Locked"),
             verbatimTextOutput("locked"),
             textInput("item", label = h3("Item to append or select"), value = ""),
             actionButton("append", label = "Append to dropzone"),
             actionButton("remove", label = "Remove selected"),
             actionButton("select", label = "Select"),
             actionButton("unselect", label = "Unselect")
      ),
      column(6,
             h3("Dropzones"),
             selectInput("whichzone",
                         label = "Which Zone?",
                         choices = c(Steve = "stevezone", Fred = "fredzone"),
                         selected = "Steve"),
             uiOutput("pickedzone")
      )
    )
  ),
  server = function(input, output, session) {
    output$showme <- renderText({
      paste(paste("Steve: ", paste(setdiff(input$stevezone, input$stevezone_invisible), collapse=" ")),
            paste("Fred: ", paste(setdiff(input$fredzone, input$fredzone_invisible), collapse=" ")),
            sep = "\n")
    })

    output$selected <- renderText({
      paste(paste("Steve: ", paste(input$stevezone_selected, collapse=" ")),
            paste("Fred: ", paste(input$fredzone_selected, collapse=" ")),
            sep = "\n")
    })

    output$invisible <- renderText({
      paste(paste("Steve: ", paste(input$stevezone_invisible, collapse=" ")),
            paste("Fred: ", paste(input$fredzone_invisible, collapse=" ")),
            sep = "\n")
    })

    output$locked <- renderText({
      paste(paste("Steve: ", paste(input$stevezone_locked, collapse=" ")),
            paste("Fred: ", paste(input$fredzone_locked, collapse=" ")),
            sep = "\n")
    })

    thiszone <- reactive({ input$whichzone %||% "stevezone" })

    output$pickedzone <- renderUI({
      dropZoneInput(thiszone(),
                    choices = list(one = "1",
                                   two = "2",
                                   three = "3",
                                   four = "4"),
                    multivalued = TRUE,
                    selectable = TRUE,
                    togglevis = TRUE,
                    togglelock = TRUE,
                    placeholder = "Drop items here",
                    presets = list(values = isolate(input[[thiszone()]]),
                                   selected = isolate(input[[paste0(thiszone(), '_selected') %||% NULL]]),
                                   invisible = isolate(input[[paste0(thiszone(), '_invisible') %||% NULL]]),
                                   locked = isolate(input[[paste0(thiszone(), '_locked') %||% NULL]])))
    })

    observeEvent(input$append, {
      dndselectr::appendToDropzone(session, input$item, input$whichzone)
    })

    observeEvent(input$remove, {
      dndselectr::removeSelected(session, input$whichzone)
    })

    observeEvent(input$select, {
      dndselectr::select(session, input$item, input$whichzone)
    })

    observeEvent(input$unselect, {
      dndselectr::unselect(session, input$whichzone)
    })
  }
)

