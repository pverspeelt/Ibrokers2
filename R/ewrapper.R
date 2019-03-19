#' eWrapper closure for message decoding
#'
#' @return an eWrapper closure
#' @export
#'
#' @examples
#' Examples here
eWrapper <- function() {
  
  # .Data <- new.env()
  # get.Data <- function(x) { 
  #   get(x, .Data)
  # }
  #   
  # assign.Data <- function(x, value) {
  #   assign(x, value, .Data)
  # }
  # 
  # remove.Data <- function(x) {
  #   remove(x, .Data)
  # }
  
  # create eWrapper functions ----------  
  error_messages <- function(curMsg, msg, timestamp, file, tws_con, ...) {
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
  
  nextValidId <- function(curMsg, msg, ...) {
    as.integer(msg[2])
  }
  
  managed_accounts <- function(curMsg, msg, ...) {
    count <- counter()
    version <- as.integer(msg[count()])
    accounts <- unlist(strsplit(msg[count()], split = ",", fixed = TRUE))
    accounts
  }
  
  current_time <- function(curMsg, msg, ...) {
    count <- counter()
    version <- as.integer(msg[count()])
    current_time <- structure(as.numeric(msg[count()]), class="POSIXct")
  }
  # create eWrapper list ----------
  eW <- list(
    # .Data = .Data,
    # get.Data = get.Data,
    # assign.Data = assign.Data,
    # remove.Data = remove.Data,
    error_messages = error_messages,
    nextValidId  =  nextValidId,
    managed_accounts = managed_accounts,
    current_time = current_time
  )
  
  class(eW) <- "eWrapper"
  invisible(eW)
}
