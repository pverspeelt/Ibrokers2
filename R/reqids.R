#' Request next valid Id
#' 
#' Requests the next valid order ID at the current moment.
#'
#' @param tws_con a valid tws_con connection object.
#'
#' @return returns the next valid Id.
#' @export
#'
#' @examples
#' \dontrun{
#' tws_con <- tws_connect()
#' reqIds(tws_con)
#' }
reqIds <- function(tws_con) {
  
  if (!is_tws_connection(tws_con)) {    
    stop("not a 'tws' connection", call. = FALSE)
  }
  
  VERSION <- "1"
  numIds <- 1L
  
  out_msg <- c(.twsOutgoingMSG$REQ_IDS,
               VERSION,
               make_field(numIds))
  
  writeBin(out_msg, tws_con$con)

  
  while (TRUE) {
    socketSelect(list(tws_con$con), FALSE, 0.1)
    curMsg <- readBin(tws_con$con, "character", 1)
    nextValidID <- process_messages(curMsg, tws_con)
    if (curMsg == .twsIncomingMSG$NEXT_VALID_ID)
      break
  }
  return(nextValidID)
}


#' @keywords internal
processNextValidIdMsg <- function(msg) {
  count <- counter()
  version <- as.integer(msg[count()])
  nextValidID <- as.integer(msg[count()])
}