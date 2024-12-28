mod_team_display_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # includeCSS("inst/app/www/card-reveal-full-screen.css"),
    # card(
    #   card_header("my header"),
    #   card_body(
    #     class = "card-reveal-full-screen", 
    #     textOutput(ns("full_screen_output1")) # Updated ID
    #   ),
    #   full_screen = TRUE
    # ),
    # card(
    #   card_header("header 2"),
    #   card_body(textOutput(ns("full_screen_output2")))
    # )
  )
}

#' team_display Server Functions
#'
#' @noRd 
mod_team_display_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render the text for the first textOutput
    output$full_screen_output1 <- renderText({
      "Full Screen Enabled in Section 1!!!"
    })
    
    # Render the text for the second textOutput
    output$full_screen_output2 <- renderText({
      "Full Screen Enabled in Section 2!!!"
    })
  })
}
