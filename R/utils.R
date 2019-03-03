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
  if( isOpen(x[[1]])) {
    cat('<tws_connection,',x$clientId,' @ ',
        as.character(x$connected.at),', nextId=',x$nextValidId,'>\n', sep="")
  } else
    cat('<tws_connection, CLOSED>\n')
}
