#' Establish a connection to TWS
#'
#' @param clientId 
#' @param host 
#' @param port 
#' @param verbose 
#' @param timeout 
#' @param filename 
#' @param blocking 
#'
#' @return A tws_con object.
#' @export
#'
#' @examples
#' \dontrun{
#' tws <- tws_connect()
#' tws_disconnect(tws)
#' }
tws_connect <-
  function (clientId = 1L, host = "localhost", port = 7496, verbose = TRUE,
            timeout = 5, filename = NULL, blocking = .Platform$OS.type ==
              "windows") {
    # TODO: documentation
    # TODO: tests
    
    
    # from ib_insync
    # MaxClientVersion = 148
    # minVer = ibapi.server_versions.MIN_CLIENT_VER
    # maxVer = min(
    #   self.MaxClientVersion, ibapi.server_versions.MAX_CLIENT_VER)
    
    # from client.py
    # def startApi(self):
    #   """  Initiates the message exchange between the client application and
    # the TWS/IB Gateway. """
    # 
    # self.logRequest(current_fn_name(), vars())
    # 
    # if not self.isConnected():
    #   self.wrapper.error(NO_VALID_ID, NOT_CONNECTED.code(),
    #                      NOT_CONNECTED.msg())
    # return
    # 
    # VERSION = 2
    # 
    # msg = make_field(OUT.START_API) \
    # + make_field(VERSION)    \
    # + make_field(self.clientId)
    # 
    # if self.serverVersion() >= MIN_SERVER_VER_OPTIONAL_CAPABILITIES:
    #   msg += make_field(self.optCapab)
    # 
    # self.sendMsg(msg)
    
    
    # startapi needs to be called after connection. 
    # 
    # v100prefix = "API\0"
    # v100version = "v%d..%d" % (MIN_CLIENT_VER, MAX_CLIENT_VER)
    # #v100version = "v%d..%d" % (MIN_CLIENT_VER, 101)
    # msg = comm.make_msg(v100version)
    # logger.debug("msg %s", msg)
    # msg2 = str.encode(v100prefix, 'ascii') + msg
    # logger.debug("REQUEST %s", msg2)
    # self.conn.sendMsg(msg2)
  
    
    start_api <- function (open_connection, clientId)
      # needs to write start_api, version, clientid 
      # question, send 3 writes or just 1?
    {
      # if (!is.tws_connection(conn))
      #   stop("requires tws_conection object")
      con <- open_connection[["con"]]
      
      VERSION <- "2"
      START_API <- .twsOutgoingMSG$START_API
      
      writeBin(START_API, con) 
      writeBin(VERSION, con)
      writeBin(as.character(clientId), con)
    }
    
    
    
    if (is.null(getOption("digits.secs"))) 
      options(digits.secs = 6)
    if (is.character(clientId))
      filename <- clientId
    
    # open connection ----------
    if (is.null(filename)) {
      start.time <- Sys.time()
      sock_con <- socketConnection(host = host, port = port, open = "ab",
                            blocking = blocking)
      on.exit(close(sock_con))
      
      if (!isOpen(sock_con)) {
        close(sock_con)
        stop(paste("couldn't connect to TWS on port", port))
      }
      
      # TODO: sending client version v100Plus protocol
      # can this be used with R?
      # 
      # if (.client_version$MIN_CLIENT_VER < .client_version$MAX_CLIENT_VER) {
      #   msg <- sprintf("v%s..%s", .client_version$MIN_CLIENT_VER, .client_version$MAX_CLIENT_VER)
      # }
      # for now using client_version 66 as defined in c++ and java when not using v100Plus protocol
      # 
      # 
     
      
      CLIENT_VERSION <- "66"
      writeBin(c(CLIENT_VERSION), sock_con)
      
      SERVER_VERSION <- NEXT_VALID_ID <- CONNECTION_TIME <- NULL
      # Server Version and connection time
      while (TRUE) {
        if (!is.null(CONNECTION_TIME))
          break
        if (!socketSelect(list(sock_con), FALSE, 0.1))
          next
        curMsg <- readBin(sock_con, character(), 2L)
        #cat(curMsg,'\n')
        
        if (is.null(SERVER_VERSION)) {
          SERVER_VERSION <- curMsg[1]
          CONNECTION_TIME <- curMsg[2]
          next
        }
      }
      
      on.exit() # connection succeeded
      
      tws_con <- new.env()
      tws_con$con <- sock_con
      tws_con$clientId <- clientId
      #tws_con$nextValidId <- NEXT_VALID_ID
      tws_con$port <- port
      tws_con$server_version <- SERVER_VERSION
      tws_con$connected_at <- CONNECTION_TIME
      tws_con$connected <- is_connection_open(tws_con$con)
      class(tws_con) <- c("tws_con", "environment")

      # tws needs an API call now
      start_api(tws_con, clientId)  

      # TODO: Get the NEXT_VALID_ID.
      # tws_con$nextValidId <- NEXT_VALID_ID <- as.integer(reqIds(tws_con, 1))

      return(tws_con)
      } else {
        fh <- file(filename, open = "r")
        dat <- scan(fh, what = character(), quiet = TRUE)
        close(fh)
        tmp <- tempfile()
        fh <- file(tmp, open = "ab")
        writeBin(dat, fh)
        close(fh)
        s <- file(tmp, open = "rb")
        tws_con <- new.env()
        tws_con$con <- s
        tws_con$clientId <- NULL
        tws_con$nextValidId <- NULL
        tws_con$port <- NULL
        tws_con$server_version <- NULL
        tws_con$connected_at <- filename
        class(tws_con) <- c("tws_playblack", "tws_con", "environment")
        return(tws_con)
      }
  }


#' @rdname tws_connect
#' @export
is_tws_connection <- function(x) {
  inherits(x, "tws_con")
}


#' @rdname tws_connect
#' @export
tws_disconnect <- function(tws_con) {
  if (!is_tws_connection(tws_con)) {    
    stop("not a 'tws' connection", call. = FALSE)
  }
  
  if (is_tws_connection_open(tws_con)) {
    tws_con$connected <- FALSE
    tws_con$connected_at <- NULL
    close(tws_con$con)
  } else {
    glue("The connection to tws is already closed.")
  }
}

#' @rdname tws_connect
#' @export
is_tws_connected <- function(tws_con){
  if (!is_tws_connection(tws_con)) {
    stop("not a 'tws' connection", call. = FALSE)
  }
  
  if (is_tws_connection_open(tws_con$con)){
    glue("connection to tws is open and was opened at: {tws_con$connected_at}.")
  } else {
    glue("connection to tws is closed.")
  }
}
