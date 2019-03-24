#' @keywords internal
processErrMsgMsg <- function(msg, tws_con) {
  count <- counter()
  version <- as.integer(msg[count()])
  id <- as.integer(msg[count()])
  error_code <- as.integer(msg[count()])
  error_message <- msg[count()]
  
  if (error_code == 1100)
    tws_con$connected <- FALSE
  if (error_code %in% c(1101, 1102))
    tws_con$connected <- TRUE
  print(glue("TWS message: {error_code} - {error_message}"))
}