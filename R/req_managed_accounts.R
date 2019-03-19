#' Request managed accounts list
#' 
#' Important: whenever your TWS user name handles more than a single account, 
#' you will be forced to specify the account Id to which the order needs 
#' to be allocated. Failure to do so will result in the order being rejected 
#' since the TWS cannot assign the order to a default account.
#'
#' @param con a valid tws_con object.
#'
#' @return prints the managed accounts that a TWS user name handles when connected to TWS.
#' @export
#'
#' @examples
#' example here
req_managed_accounts <- function(con) {
  
  if (!is_tws_connection(con)) {    
    stop("not a 'tws' connection", call. = FALSE)
  }
  
  con <- con$con
  
  VERSION <- "1"
  
  out_msg <- c(.twsOutgoingMSG$REQ_MANAGED_ACCTS,
               VERSION)
  
  writeBin(out_msg, con)
  
  ew <- eWrapper()
  
  while (TRUE) {
    socketSelect(list(con), FALSE, 0.1)
    curMsg <- readBin(con, "character", 1)
    managed_accounts <- process_messages(curMsg, con, eWrapper = ew)
    if (curMsg == .twsIncomingMSG$MANAGED_ACCTS)
      break
  }
  
  if (length(managed_accounts) > 1) {
    print(glue('Your TWS user name handles {length(managed_accounts)} accounts.',
                 ' Read the documentation with the req_managed_accounts function.'))
  }
  print(glue("you are connected to account: {managed_accounts}"))
  managed_accounts
}