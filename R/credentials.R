#' Retrieve FLUXNET / AmeriFlux credentials from environment variables
#'
#' Returns a named list of credentials read from environment variables.
#' Stops if required variables are missing so callers get an informative error
#' rather than silently passing empty strings to the API.
#'
#' @return A named list with elements `user_name`, `user_email`, and
#'   `intended_use`.
#' @export
fluxnet_credentials <- function() {
  user_name <- Sys.getenv("AMERIFLUX_USER_NAME", unset = NA_character_)
  user_email <- Sys.getenv("AMERIFLUX_USER_EMAIL", unset = NA_character_)
  intended_use <- Sys.getenv("AMERIFLUX_INTENDED_USE", unset = "1")

  if (is.na(user_name) || nchar(trimws(user_name)) == 0) {
    stop("AMERIFLUX_USER_NAME is not set. Set it as a Codespace Secret or in .env.")
  }
  if (is.na(user_email) || nchar(trimws(user_email)) == 0) {
    stop("AMERIFLUX_USER_EMAIL is not set. Set it as a Codespace Secret or in .env.")
  }

  list(
    user_name    = user_name,
    user_email   = user_email,
    intended_use = intended_use
  )
}
