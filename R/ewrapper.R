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
  errorMessage <- function(curMsg, msg, timestamp, file, tws_con, ...) {
    if (msg[3] == "1100")
      tws_con$connected <- FALSE
    if (msg[3] %in% c("1101", "1102"))
      tws_con$connected <- TRUE
    cat("TWS Message:", msg, "\n")
  }
  
  nextValidId <- function(curMsg, msg, ...) {
    as.integer(msg[2])
  }
  
  
  # create eWrapper list ----------
  eW <- list(
    # .Data = .Data,
    # get.Data = get.Data,
    # assign.Data = assign.Data,
    # remove.Data = remove.Data,
    errorMessage = errorMessage,
    nextValidId  =  nextValidId
  )
  
  class(eW) <- "eWrapper"
  invisible(eW)
}
