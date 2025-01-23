#' account UI Function
#'
#' @description A shiny Module displaying user account status and business information for payment processing.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_account_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "account-section",
      hr(),
      h4("Business Information"),
      div(
        h5("Description of Services"),
        p("We offer detailed ultimate frisbee performance tracking, team management tools, leaderboards, and advanced analytics.")
      ),
      div(
        h5("Transaction Currency"),
        p("All transactions are processed in USD unless stated otherwise.")
      ),
      div(
        h5("Customer Service Contact Details"),
        p("Phone: 8019955983"),
        p("Email: braden.ultimate@gmail.com"),
        p("Address: 81 Thorndike St, Cambridge, MA, 02141")
      ),
      div(
        h5("Legal or Export Restrictions"),
        p("Our services are available globally, with no export restrictions. However, users must comply with local laws regarding online transactions.")
      ),
      div(
        h5("Consumer Data Privacy Policy"),
        p("We value your privacy and handle all personal data in accordance with industry standards.")
      ),
      div(
        h5("Security Capabilities and Policy for Transmission of Payment Card Details"),
        p("All payment card details are encrypted and transmitted securely using Stripe’s payment processing technology. We do not store your card information on our servers.")
      )
    )
  )
}
    
#' account Server Functions
#'
#' @noRd 
mod_account_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}
    
## To be copied in the UI
# mod_account_ui("account_1")
    
## To be copied in the server
# mod_account_server("account_1")
