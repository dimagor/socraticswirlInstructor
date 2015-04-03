## Parse API functions

base_URL <- "https://api.parse.com"

Parse_headers <- function() {
  header_strs <- c('X-Parse-Application-Id' = Sys.getenv("PARSE_APPLICATION_ID"),
                   'X-Parse-REST-API-Key' = Sys.getenv("PARSE_API_KEY"))

  if (any(header_strs == "")) {
    stop("DEVELOPMENT: must set PARSE_APPLICATION_ID and PARSE_API_KEY environment variables")
  }

  # if currently logged in:
  user <- getOption("parse_user")
  if (!is.null(user)) {
    session_token <- user$sessionToken
    header_strs <- c(header_strs, "X-Parse-Session-Token" = session_token)
  }

  httr::add_headers(.headers = header_strs)
}


Parse_GET <- function(path, ...) {
  req <- httr::GET(base_URL, path = paste0("1/", path), Parse_headers(), ...)
  process_Parse(req)
}


#' Perform a POST request to parse
Parse_POST <- function(path, body, ...) {
  req <- httr::POST(base_URL, path = paste0("1/", path),
                    body = rjson::toJSON(body),
                    encode = "json", Parse_headers(), ...)

  process_Parse(req)
}


#' process a request object from Parse
process_Parse <- function(req) {
  txt <- httr::content(req, as = "text")
  j <- jsonlite::fromJSON(txt)

  if (!is.null(j$error)) {
    stop("Error ", j$code, ": ", j$error)
  }

  if ("results" %in% names(j)) {
    j <- j$results
  }

  for (col in names(j)) {
    if (grepl("At$", col)) {
      j[[col]] <- as.POSIXct(j[[col]], origin = "1970-01-01", format="%Y-%m-%dT%H:%M:%S")
    }
  }

  j
}


Parse_create <- function(class_name, ...) {
  body <- list(...)
  Parse_POST(paste0("classes/", class_name), body)
}

#' retrieve one or more objects from Parse
#'
#' @param class_name
#' @param object_id if provided, a specific object ID to retrieve
#' @param ... fields to query by
#'
#' @export
Parse_retrieve <- function(class_name, object_id, ...) {
  url <- paste("classes", class_name, sep = "/")

  # as of now, accepts only exact queries
  params <- list(...)
  if (length(params) > 0) {
    q <- list(where = rjson::toJSON(params))
  } else {
    q <- NULL
  }
  Parse_GET(url, query = q)
}


#' create a new user
#'
#' @return a list comprising the user's session
#'
#' @export
Parse_signup <- function(username, password, ...) {
  ret <- Parse_POST("users/", list(username = username, password = password, ...))
  options(parse_user = ret)
  invisible(ret)
}


#' Log a user into Parse
#'
#' This submits a login
#'
#' @param username username
#' @param password password
#' @param ... additional arguments, such as email
#'
#' @return a list comprising the logged in user's session
#'
#' @export
Parse_login <- function(username, password, ...) {
  ret <- Parse_GET("login/", query = list(username = username, password = password))
  options(parse_user = ret)
  invisible(ret)
}


Parse_current_user <- function() {
  Parse_GET("users/me")
}


#' reset a user's password (if they have email)
#'
#' @param email User's e-mail address
#'
#' @export
Parse_password_reset <- function(email) {
  Parse_POST("requestPasswordReset", email = email)
}
