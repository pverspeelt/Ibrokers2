#' Request managed accounts list
#' 
#' Important: whenever your TWS user name handles more than a single account, 
#' you will be forced to specify the account Id to which the order needs 
#' to be allocated. Failure to do so will result in the order being rejected 
#' since the TWS cannot assign the order to a default account.
#'
#' @param tws_con a valid tws_con object.
#'
#' @return prints the managed accounts that a TWS user name handles when connected to TWS.
#' @export
#'
#' @examples
#' example here
req_managed_accounts <- function(tws_con) {
  
  if (!is_tws_connection(tws_con)) {    
    stop("not a 'tws' connection", call. = FALSE)
  }
  
  VERSION <- "1"
  
  out_msg <- c(.twsOutgoingMSG$REQ_MANAGED_ACCTS,
               VERSION)
  
  writeBin(out_msg, tws_con$con)

  while (TRUE) {
    socketSelect(list(tws_con$con), FALSE, 0.1)
    curMsg <- readBin(tws_con$con, "character", 1)
    managed_accounts <- process_messages(curMsg, tws_con)
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

#' @keywords internal
processManagedAcctsMsg <- function(msg){
  count <- counter()
  version <- as.integer(msg[count()])
  accounts <- unlist(strsplit(msg[count()], split = ",", fixed = TRUE))
  accounts
}