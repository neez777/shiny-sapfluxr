#' Helper functions for notifications using SweetAlert
#'
#' These functions provide consistent notification behaviour across the app

#' Show success notification with auto-close
notify_success <- function(session, title, text, timer = 3000) {
  shinyWidgets::sendSweetAlert(
    session = session,
    title = title,
    text = text,
    type = "success",
    timer = timer
  )
}

#' Show error notification (no auto-close)
notify_error <- function(session, title, text) {
  shinyWidgets::sendSweetAlert(
    session = session,
    title = title,
    text = text,
    type = "error"
  )
}

#' Show warning notification with auto-close
notify_warning <- function(session, title, text, timer = 3000) {
  shinyWidgets::sendSweetAlert(
    session = session,
    title = title,
    text = text,
    type = "warning",
    timer = timer
  )
}

#' Show info notification with auto-close
notify_info <- function(session, title, text, timer = 3000) {
  shinyWidgets::sendSweetAlert(
    session = session,
    title = title,
    text = text,
    type = "info",
    timer = timer
  )
}

#' Show progress notification (no buttons, no auto-close)
notify_progress <- function(title, text) {
  shinyWidgets::show_alert(
    title = title,
    text = text,
    type = "info",
    showConfirmButton = FALSE,
    timer = NULL
  )
}

#' Close any open SweetAlert
close_notify <- function(session) {
  shinyWidgets::closeSweetAlert(session = session)
}
