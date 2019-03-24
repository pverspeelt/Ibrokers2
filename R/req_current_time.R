#' Request the current time from the IB server
#' 
#' TWS's current time. TWS is synchronized with the server (not local computer) 
#' using NTP and this function will receive the current time in TWS.
#'
#' @param tws_con a valid tws connection.
#'
#' @return returns TWS's current time.
#' @export
#'
#' @examples
#' example here
req_current_time <- function(tws_con) {
  
  if (!is_tws_connection(tws_con)) {    
    stop("not a 'tws' connection", call. = FALSE)
  }
  
  
  VERSION <- "1"
  
  out_msg <- c(.twsOutgoingMSG$REQ_CURRENT_TIME,
               VERSION)
  
  writeBin(out_msg, tws_con$con)
  
  ew <- eWrapper()
  
  while (TRUE) {
    socketSelect(list(tws_con$con), FALSE, 0.1)
    curMsg <- readBin(tws_con$con, "character", 1)
    current_time <- process_messages(curMsg, tws_con)
    if (curMsg == .twsIncomingMSG$CURRENT_TIME)
      break
  }
  current_time
}

#' @keywords internal
processCurrentTimeMsg <- function(msg){
  count <- counter()
  version <- as.integer(msg[count()])
  current_time <- structure(as.numeric(msg[count()]), class="POSIXct")
}