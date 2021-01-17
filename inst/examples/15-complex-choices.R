library(shiny)
library(dndselectr)

createItem <- function(text) {
  tagList(
    HTML("<style>
         .blue { color:#fff; background: #00f; }
         .blue i { margin-left: 5px; }
         .ds-dropzone .ds-dropoption { cursor: default; }
         .ds-handle { cursor: move; }
         </style>"),
    div(class = "blue",
        icon("chalkboard-teacher", class="ds-handle"),
        text)
  )
}

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
             dropZoneInput("dropzone",
                           choices = list(one = createItem("1"),
                                          two = createItem("2"),
                                          three = createItem("3"),
                                          four = createItem("4")),
                           presets = c("one"))
      )
    )
  ),
  server = function(input, output) {
    output$showme <- renderPrint({ input$dropzone })
  }
)

