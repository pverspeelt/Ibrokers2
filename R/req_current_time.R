#' Request the current time from the IB server
#'
#' @param con 
#'
#' @return returns the next valid Id that can be used as an order id.
#' @export
#'
#' @examples
#' example here
req_current_time <- function(con) {
  
  if (!is_tws_connection(con)) {    
    stop("not a 'tws' connection", call. = FALSE)
  }
  
  
  con <- con$con
  
  VERSION <- "1"
  
  out_msg <- c(.twsOutgoingMSG$REQ_CURRENT_TIME,
               VERSION)
  
  writeBin(out_msg, con)
  
  ew <- eWrapper()
  
  while (TRUE) {
    socketSelect(list(con), FALSE, 0.1)
    curMsg <- readBin(con, "character", 1)
    current_time <- process_messages(curMsg, con, eWrapper = ew)
    if (curMsg == .twsIncomingMSG$CURRENT_TIME)
      break
  }
  current_time
}