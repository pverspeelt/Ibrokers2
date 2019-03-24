#' Process messages from TWS
#'
#' @param curMsg 
#' @param tws_con a valid tws connection object
#' @param ... 
#'
#' @return used for processing incoming messages. Called for it's side-effects.
#' @keywords internal
#'
#' @examples
#' examples here
process_messages <- function(curMsg, tws_con, ...)
{
  if (curMsg == .twsIncomingMSG$ERR_MSG) {
    msg <- readBin(tws_con$con, "character", 4L)
    processErrMsgMsg(msg, tws_con)
  } else 
  if (curMsg == .twsIncomingMSG$NEXT_VALID_ID) {
    msg <- readBin(tws_con$con, "character", 2L)
    processNextValidIdMsg(msg)
  } else 
  if (curMsg == .twsIncomingMSG$MANAGED_ACCTS) {
    msg <- readBin(tws_con$con, "character", 2L)
    processManagedAcctsMsg(msg)
  } else
  if (curMsg == .twsIncomingMSG$CURRENT_TIME) {
    msg <- readBin(tws_con$con, "character", 2L)
    processCurrentTimeMsg(msg)
  } else
  if (curMsg == .twsIncomingMSG$CONTRACT_DATA) {
    msg <- readBin(tws_con$con, "character", 31L)
    processContractDataMsg(msg, tws_con)
  } else
  if (curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {
    msg <- readBin(tws_con$con, "character", 2L)
    processContractDataEndMsg(msg)
  }  
  else {
    # default message handler
    warning(glue("Unknown incoming message: {curMsg}."), call.=FALSE)
  }
  # end of messages
}
