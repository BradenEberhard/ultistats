#' Create a Reload Button UI Component
#'
#' This function generates a UI component that displays a button to reload an API.
#' The button is styled with the Bootstrap `btn-outline-primary` class and includes
#' a sync icon from Font Awesome.
#'
#' @param ns A function used to namespace the input IDs (required in a modular app).
#' @return A `shiny.tag` object representing the action button UI component.
#' @export

reload_button_ui <- function(id, label = "Reload API", icon = NULL) {
  if (is.null(icon)) {
    icon <- icon("sync-alt")  # Default icon
  }
  
  actionButton(
    inputId = id, 
    label = label,
    icon = icon,
    class = "btn-outline-primary"
  )
}

