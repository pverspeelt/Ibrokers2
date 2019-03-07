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
is_tws_connection_open <- function(con) {
  if(inherits(try(isOpen(con), silent=TRUE), 'try-error')) {
    FALSE
  } else TRUE 
}
