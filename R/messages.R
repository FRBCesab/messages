#' Pre-formatted messages for a custom user interface
#'
#' @description
#' These functions are used to construct an user interface _a la_ `usethis`.
#'
#' These `msg_*` functions can be broken down into two main categories:
#'
#' - **block styles**: `msg_line()`, `msg_rule()`, `msg_done()`, `msg_todo()`,
#'   `msg_info()`, `msg_warn()`, `msg_oops()`.
#'
#' - **inline styles**: `msg_field()`, `msg_value()`, `msg_code()`.
#'
#' @param ... One or several expression (character or vector object).
#'
#' @note
#' Messages can be suppressed by using `options(messages = FALSE)`.
#' Messages can be reactivated by using: `options(messages = TRUE)`.
#'
#' @return Print a success message.
#'
#' @export
#'
#' @examples
#' ## Block messages ----
#'
#' msg_done("A success message")
#' msg_todo("A to do message")
#' msg_info("An informational message")
#' msg_warn("A warning message")
#' msg_oops("An error message")
#'
#' msg_line()
#' msg_line("A message")
#'
#' msg_rule("Left message")
#' msg_rule("Left message", , "Right message")
#' msg_rule("Left message", right = "Right message")
#' msg_rule(, "Center message")
#' msg_rule(center = "Center message")
#'
#' ## Inline messages ----
#'
#' msg_done("The variable", msg_field("x"), "has been set to", msg_value(1))
#' msg_todo("Please use the function:", msg_code("msg_rule()"))
#' msg_line("Thanks for using", msg_code("messages"))
#' msg_line(msg_code("msg_rule()"))

msg_done <- function(...) {

  if (is.null(options()$"messages") || options()$"messages") {

    x <- paste(...)
    x <- msg_bullet(x, crayon::green(cli::symbol$"tick"))
    cli::cat_line(x)
  }

  invisible(NULL)
}



#' @describeIn msg_done Print a to do message.
#'
#' @export

msg_todo <- function(...) {

  if (is.null(options()$"messages") || options()$"messages") {

    x <- paste(...)
    x <- msg_bullet(x, crayon::red(cli::symbol$"bullet"))
    cli::cat_line(x)
  }

  invisible(NULL)
}



#' @describeIn msg_done Print an informational message.
#'
#' @export

msg_info <- function(...) {

  if (is.null(options()$"messages") || options()$"messages") {

    x <- paste(...)
    x <- msg_bullet(x, crayon::yellow(cli::symbol$"info"))
    cli::cat_line(x)
  }

  invisible(NULL)
}



#' @describeIn msg_done Print a warning message.
#'
#' @export

msg_warn <- function(...) {

  if (is.null(options()$"messages") || options()$"messages") {

    x <- paste(...)
    x <- msg_bullet(x, crayon::bold(crayon::red(cli::symbol$"warning")))
    cli::cat_line(x)
  }

  invisible(NULL)
}



#' @describeIn msg_done Print an error message.
#'
#' @export

msg_oops <- function(...) {

  if (is.null(options()$"messages") || options()$"messages") {

    x <- paste(...)
    x <- msg_bullet(x, crayon::bold(crayon::red(cli::symbol$"cross")))
    cli::cat_line(x)
  }

  invisible(NULL)
}



#' @describeIn msg_done Print an (non-)empty message.
#'
#' @export

msg_line <- function(...) {

  if (is.null(options()$"messages") || options()$"messages") {

    x <- paste(...)
    cli::cat_line(x)
  }

  invisible(NULL)
}



#' @describeIn msg_done Print a rule message.
#'
#' @export

msg_rule <- function(...) {

  if (is.null(options()$"messages") || options()$"messages") {

    cli::cat_rule(...)
  }

  invisible(NULL)
}



#' @describeIn msg_done Returns a customized code expression.
#'
#' @export

msg_field <- function(...) {

  x <- unlist(list(...))

  invisible(paste0(crayon::green(x), collapse = ", "))
}



#' @describeIn msg_done Returns a customized variable value.
#'
#' @export

msg_value <- function(...) {

  x <- unlist(list(...))

  if (is.character(x)) {
    x <- encodeString(x, quote = "'")
  }

  invisible(paste0(crayon::blue(x), collapse = ", "))
}



#' @describeIn msg_done Returns a customized code expression.
#'
#' @export

msg_code <- function(...) {

  x <- unlist(list(...))

  if (is.character(x)) {
    x <- encodeString(x, quote = "`")
  }

  invisible(paste0(crayon::silver(x), collapse = ", "))
}



#' @noRd

msg_bullet <- function(x, bullet = cli::symbol$"bullet") {

  bullet <- paste0(bullet, " ")
  msg_indent(x, bullet, "  ")
}



#' @noRd

msg_indent <- function (x, first = "  ", indent = first) {

  x <- gsub("\n\\s", "\n", x)
  x <- gsub("\n", paste0("\n", indent), x)
  paste0(first, x)
}
