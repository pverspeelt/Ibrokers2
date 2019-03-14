#' Request next valid Id
#'
#' @param con 
#' @param numIds 
#'
#' @return returns the next valid Id that can be used as an order id.
#' @export
#'
#' @examples
#' example here
reqIds <- function(con, numIds = 1L) {
  
  if (!is_tws_connection(con)) {    
    stop("not a 'tws' connection", call. = FALSE)
  }
  
  if (is.character(numIds)) { 
    stop(glue::glue("numIds is a character. 
                    Please use an integer value"))
  } else if (numIds%%1 == 0) {
    numIds <- as.integer(numIds) 
    } else {
      stop(glue::glue("numIds is not an integer.
                      Please use an integer value"))
    } 

  con <- con$con
  
  VERSION <- "1"
  
  out_msg <- c(.twsOutgoingMSG$REQ_IDS,
               VERSION,
               make_field(numIds))
  
  writeBin(out_msg, con)

  ew <- eWrapper()
  
  while (TRUE) {
    socketSelect(list(con), FALSE, 0.1)
    curMsg <- readBin(con, "character", 1)
    nextValidID <- process_messages(curMsg, con, eWrapper = ew)
    if (curMsg == .twsIncomingMSG$NEXT_VALID_ID)
      break
  }
  return(nextValidID)
}