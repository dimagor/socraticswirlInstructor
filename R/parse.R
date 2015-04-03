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
Parse_POST <- function(path, body, to_json = TRUE, ...) {
  if (to_json) {
    body <- rjson::toJSON(body)
  }
  req <- httr::POST(base_URL, path = paste0("1/", path),
                    body = body, encode = "json",
                    Parse_headers(), ...)

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
  ret <- Parse_POST(paste0("classes/", class_name), body)
  as_Parse_class(ret, class_name)
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
  ret <- Parse_GET(url, query = q)
  as_Parse_class(ret, class_name)
}

as_Parse_class <- function(x, class_name) {
  attr(x, "class_name") <- class_name
  x
}


#' create a new user
#'
#' @return a Parse user object
#'
#' @export
Parse_signup <- function(username, password, ...) {
  ret <- Parse_POST("users/", list(username = username, password = password, ...))
  ret <- as_Parse_class(ret, "_User")
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
  ret <- as_Parse_class(ret, "_User")
  options(parse_user = ret)
  invisible(ret)
}


Parse_current_user <- function() {
  as_Parse_class(Parse_GET("users/me"), "_User")
}


#' reset a user's password (if they have email)
#'
#' @param email User's e-mail address
#'
#' @export
Parse_password_reset <- function(email) {
  Parse_POST("requestPasswordReset", email = email)
}


#' Upload a file to Parse
#'
#' @param name name to save the file to
#' @param path path to file to upload
#' @param type type of file (e.g.)
#'
#' @return a file object, containing a name and URL
Parse_upload_file <- function(name, path, type = NULL) {
  upfile <- httr::upload_file(path, type = type)
  ret <- Parse_POST(file.path("files", name), body = upfile, to_json = FALSE)
  class(ret) <- c("file", class(ret))
  ret
}


#' convert a Parse object to a pointer
convert_pointer <- function(x, ...) {
  UseMethod("convert_pointer")
}

convert_pointer.default <- function(x, ...) {
  list("__type" = "Pointer",
       "className" = attr(x, "class_name"),
       "objectId" = x$objectId)
}

convert_pointer.file <- function(x, ...) {
  list("__type" = "File",
       "name" = x$name)
}
