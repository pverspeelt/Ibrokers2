#' Process messages from TWS
#'
#' @param curMsg 
#' @param con 
#' @param eWrapper 
#' @param ... 
#'
#' @return used for processing incoming messages. Called for it's side-effects.
#' @keywords internal
#'
#' @examples
#' examples here
process_messages <- function(curMsg, con, eWrapper, ...)
{
  if (curMsg == .twsIncomingMSG$ERR_MSG) {
    msg <- readBin(con, "character", 4L)
    eWrapper$error_messages(curMsg, msg, ...)
  } else 
  if (curMsg == .twsIncomingMSG$NEXT_VALID_ID) {
    msg <- readBin(con, "character", 2L)
    eWrapper$nextValidId(curMsg, msg, ...)
  } else 
  if (curMsg == .twsIncomingMSG$MANAGED_ACCTS) {
    msg <- readBin(con, "character", 2L)
    eWrapper$managed_accounts(curMsg, msg, ...)
  } else {
    # default message handler
    warning(glue("Unknown incoming message: {curMsg}."), call.=FALSE)
  }
  # end of messages
}
