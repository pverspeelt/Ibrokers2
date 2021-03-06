#' Establish a connection to TWS
#'
#' @param clientId 
#' @param host Default connection is 127.0.0.1 for the trade work station
#' @param port Default connection is to the paper trading account on port 7497.
#' Live connection to tws is on port 7496. For the IB gateway the live port is 4001 
#' and the paper trading port is 4002.
#' @param verbose 
#' @param timeout 
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
  function (clientId = 1L, host = "127.0.0.1", port = 7497, verbose = TRUE,
            timeout = 5, blocking = .Platform$OS.type == "windows") {
    
    # TODO: documentation
    # TODO: tests

    # switch to reticulate and use the python interface? 
    # python works very fast and is async.
    # use promises in combination with future to create an async in R?
    
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
    # msg = comm.make_msg(v100version)
    # logger.debug("msg %s", msg)
    # msg2 = str.encode(v100prefix, 'ascii') + msg
    # logger.debug("REQUEST %s", msg2)
    # self.conn.sendMsg(msg2)
  

    # ibapi needs an api trigger    
    start_api <- function(tws_con, clientId) {

      con <- tws_con$con
      
      # connectOptions for now empty
      # + PACEAPI flag under review
      optCapab <- ""

      VERSION <- "2"
      
      out_msg <- c(.twsOutgoingMSG$START_API, 
                   VERSION,
                   make_field(clientId))
      if (tws_con$server_version > .server_version$MIN_SERVER_VER_OPTIONAL_CAPABILITIES) {
        out_msg <- c(out_msg, make_field(optCapab))
      }
      
      writeBin(out_msg, con)
    }
    
    
    
    if (is.null(getOption("digits.secs"))) 
      options(digits.secs = 6)

    # open connection ----------
    start.time <- Sys.time()
    sock_con <- socketConnection(host = host, port = port, open = "ab",
                            blocking = blocking)
    on.exit(close(sock_con))
      
    if (!isOpen(sock_con)) {
      close(sock_con)
      stop(glue("couldn't connect to TWS on port: {port}"), call. = FALSE)
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
     
    # send client version and receive server version  
    CLIENT_VERSION <- "66"
    writeBin(CLIENT_VERSION, sock_con)
      
    SERVER_VERSION <- NEXT_VALID_ID <- CONNECTION_TIME <- NULL
      # Server Version and connection time
    while (TRUE) {
      if (!is.null(CONNECTION_TIME))
        break
      if (!socketSelect(list(sock_con), FALSE, 0.1))
        next
      curMsg <- readBin(sock_con, "character", 2L)
      #cat(curMsg,'\n')
        
      if (is.null(SERVER_VERSION)) {
        SERVER_VERSION <- curMsg[1]
        CONNECTION_TIME <- curMsg[2]
        next
      }
      
      if (Sys.time()-start.time > timeout) {
        close(sock_con)
        stop('tws connection timed-out')
      }
    }
      
    on.exit() # connection succeeded
      
    tws_con <- new.env()
    tws_con$con <- sock_con
    tws_con$clientId <- clientId
    tws_con$port <- port
    tws_con$server_version <- as.integer(SERVER_VERSION)
    tws_con$connected_at <- CONNECTION_TIME
    tws_con$connected <- is_tws_connection_open(tws_con)
    class(tws_con) <- c("tws_con", "environment")
    
    # tws needs an API call now
    start_api(tws_con, clientId)  

    # pause a bit 
    Sys.sleep(1)

    # read and process connection messages ----------
    
    while (TRUE) {
      if (socketSelect(list(sock_con), FALSE, 1)) {
        curMsg <- readBin(sock_con, "character", 1)
        if (curMsg == .twsIncomingMSG$MANAGED_ACCTS) {
        tws_con$accounts <- process_messages(curMsg, tws_con)
        } else
          if (curMsg == .twsIncomingMSG$NEXT_VALID_ID) {
            tws_con$nextValidId <- process_messages(curMsg, tws_con)
          } else
            # print Market data farm messages
        if (curMsg == .twsIncomingMSG$ERR_MSG) {
          process_messages(curMsg, tws_con)
        }
      } else 
        break
    }

    if (length(tws_con$accounts) > 1) {
      print(glue('Your TWS user name handles {length(tws_con$accounts)} accounts.',
                 ' Read the documentation with the req_managed_accounts function'))
    }
    print(glue("you are connected to account: {tws_con$accounts}"))
    
    return(tws_con)
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
    tws_con$server_version <- 0L
    tws_con$clientId <- -1L
    tws_con$nextvalidId <- 0L
    tws_con$accounts <- NULL
    close(tws_con$con)
    glue("The connection to tws is now closed")
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
  
  if (is_tws_connection_open(tws_con)){
    glue("connection to tws is open and was opened at: {tws_con$connected_at}.")
  } else {
    glue("connection to tws is closed.")
  }
}
