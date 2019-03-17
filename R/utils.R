#' Printing tws connection
#'
#' @param x object to print
#' @param ... Other arguments passed on to individual methods.
#'
#' @export
#'
#' @examples
#' print(tws_con)
print.tws_con <- function(x, ...) {
  # TODO: rewrite to use glue and is_tws_connection_open
  if (isOpen(x[["con"]])) {
    cat('<tws_connection,',x$clientId,' @ ',
        as.character(x$connected.at),'>\n', sep="")
  } else
    cat('<tws_connection, CLOSED>\n')
}

#' @keywords internal
is_tws_connection_open <- function(tws_con) {
  if (inherits(try(isOpen(tws_con$con), silent=TRUE), 'try-error')) {
    FALSE
  } else TRUE 
}

#' @keywords internal
make_field <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) '' else as.character(x)
}

#' @keywords internal
counter <- function() {
  i <- 0
  function() {
    i <<- i + 1
  }
}
