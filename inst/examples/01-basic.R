shinyApp(
  ui = fluidPage(
    column(6,
           dragZone("dragzone", choices = list(one = "One",
                                               two = "Two",
                                               three = "Three",
                                               four = "Four"))
    ),
    column(6,
           dropZoneInput("dropzone", choices = list(one = "1",
                                                    two = "2",
                                                    three = "3",
                                                    four = "4"),
                         highlight = TRUE)
    )
  ),
  server = function(input, output) {
  }
)
