#' Request contract details
#'
#' @param contract a valid contract.
#' @param reqId the request's unique identifier. Should be a scalar integer.
#' @param tws_con a valid tws connection.
#'
#' @return a list of details for the specified contract.
#' @export
#'
#' @examples
#' \dontrun{
#' tws_con <- tws_connect()
#' ibm <- contract(symbol = "IBM", 
#'                 secType = "STK", 
#'                 currency = "USD", 
#'                 exchange = "SMART")
#' req_contract_details(ibm, tws_con)
#' }
req_contract_details <- function(contract, reqId = 1L, tws_con) {
  
  # create and send outgoing message ----------  
  out_msg <- create_reqContractDetails_msg(contract = contract,
                                           reqId = reqId,
                                           tws_con = tws_con)
  writeBin(out_msg, tws_con$con)
  
  contracts <- list()
  while (TRUE) {
    socketSelect(list(tws_con$con), FALSE, NULL) 
    curMsg <- readBin(tws_con$con, "character", 1L)
    
    if (curMsg == .twsIncomingMSG$ERR_MSG) {
      process_messages(curMsg, tws_con)
        break
      } else if (curMsg == .twsIncomingMSG$CONTRACT_DATA) {
        contracts[[length(contracts) + 1L]] <- process_messages(curMsg, tws_con)
      } else if (curMsg == .twsIncomingMSG$CONTRACT_DATA_END){
        process_messages(curMsg, tws_con)
        break      
    }
  }
  
  contracts
}


#' @keywords internal
create_reqContractDetails_msg <- function(contract, reqId, tws_con) {
  
  # stop the function or give a warning and a message that the values are set to ""?
  if (tws_con$server_version < .server_version$MIN_SERVER_VER_SEC_ID_TYPE) {
    if (nchar(contract$secIdType) != 0 || nchar(contract$secId) != 0) {
      stop(glue("Current server version {tws_con$server_version} does not support secIdType and secId parameters."), 
           call. = FALSE)
    }
  }
  if (tws_con$server_version < .server_version$MIN_SERVER_VER_TRADING_CLASS) {
    if (nchar(contract$tradingClass) != 0) {
      stop(glue("Current server version {tws_con$server_version} does not support tradingClass parameter in reqContractDetails."), 
           call. = FALSE)
    }
  }
  if (tws_con$server_version < .server_version$MIN_SERVER_VER_LINKING) {
    if (nchar(contract$primaryExchange) != 0) {
      stop(glue("Current server version {tws_con$server_version} does not support primaryExchange parameter in reqContractDetails."),
           call. = FALSE)
    }
  }
  
  VERSION = 8
  
  out_msg <- c(.twsOutgoingMSG$REQ_CONTRACT_DATA,
               make_field(VERSION))
  
  if (tws_con$server_version >= .server_version$MIN_SERVER_VER_CONTRACT_DATA_CHAIN) {
    out_msg <- c(out_msg,
                 make_field(reqId))
  }
  
  out_msg <- c(out_msg, 
               make_field(contract$conId), # srv v37 and above
               make_field(contract$symbol),
               make_field(contract$secType),
               make_field(contract$lastTradeDateOrContractMonth),
               make_field(contract$strike),
               make_field(contract$right),
               make_field(contract$multiplier)) # srv v15 and above
  
  if (tws_con$server_version >= .server_version$MIN_SERVER_VER_PRIMARYEXCH) {
    out_msg <- c(out_msg, 
                 make_field(contract$exchange),
                 make_field(contract$primaryExchange))
  } else if (tws_con$server_version >= .server_version$MIN_SERVER_VER_LINKING) {
    if (nchar(contract$primaryExchange) != 0 && 
        (contract$exchange == "BEST" || contract$exchange == "SMART")) {
      out_msg <- c(out_msg,
                   make_field(paste0(contract$exchange, ":", 
                                     contract$primaryExchange)))
    } else {
      out_msg <- c(out_msg,
                   make_field(contract$exchange))
    }
  }
  
  out_msg <- c(out_msg,
               make_field(contract$currency), 
               make_field(contract$localSymbol))
  
  if (tws_con$server_version >= .server_version$MIN_SERVER_VER_TRADING_CLASS) {
    out_msg <- c(out_msg,
                 make_field(contract$tradingClass))
  }
  
  out_msg <- c(out_msg,
               make_field(contract$includeExpired)) # srv v31 and above
  
  if (tws_con$server_version >= .server_version$MIN_SERVER_VER_SEC_ID_TYPE) {
    out_msg <- c(out_msg,
                 make_field(contract$secIdType),
                 make_field(contract$secId))
  }
}

#' @keywords internal
processContractDataMsg <- function(msg, tws_con) {
  count <- counter()
  version <- as.integer(msg[count()]) 
  
  reqId <- msg[count()]
  if (version >= 3) {
    reqId <- as.integer(reqId)
  } else reqId <- -1
  
  contract <- contractDetails(contract())
  contract$contract$symbol <- msg[count()]
  contract$contract$secType <- msg[count()]
  # decoder function available for lastTradeDateOrContractMonth
  contract$contract$lastTradeDateOrContractMonth <- msg[count()]
  contract$contract$strike <- as.numeric(msg[count()])
  contract$contract$right <- msg[count()]
  contract$contract$exchange <- msg[count()]
  contract$contract$currency <- msg[count()]
  contract$contract$localSymbol <- msg[count()]
  contract$marketName <- msg[count()]
  contract$contract$tradingClass <- msg[count()]
  contract$contract$conId <- as.integer(msg[count()])
  contract$minTick <- as.numeric(msg[count()])
  
  if (tws_con$server_version >= .server_version$MIN_SERVER_VER_MD_SIZE_MULTIPLIER) {
    contract$mdSizeMultiplier <- as.integer(msg[count()]) 
  }
  
  contract$contract$multiplier <- msg[count()]
  contract$orderTypes <- msg[count()]
  contract$validExchanges <- msg[count()]
  contract$priceMagnifier <- as.integer(msg[count()]) # ver 2 field
  
  if (version >= 4) {
    contract$underConId <- as.integer(msg[count()])
  }
  
  if (version >= 5) {
    contract$longName <- msg[count()]
    contract$contract$primaryExchange <- msg[count()]
  }
  
  if (version >= 6) {
    contract$contractMonth <- msg[count()]
    contract$industry <- msg[count()]
    contract$category <- msg[count()]
    contract$subcategory <- msg[count()]
    contract$timeZoneId <- msg[count()]
    contract$tradingHours <- msg[count()]
    contract$liquidHours <- msg[count()]
  }
  
  if (version >= 8) {
    contract$evRule <- msg[count()]
    contract$evMultiplier <- as.integer(msg[count()])
  }
  
  if (version >= 7) {
    secIdListCount <- as.integer(msg[count()])
    if (secIdListCount  > 0){
      # create list of secids
      
      tag <- NULL
      value <- NULL
      for (i in 1:secIdListCount){
        tag[i] <- msg2[count()]
        value[i] <- msg[count()]
      }
      contract$secIdList <- list(tag = tag,
                                 value = value)
    }
  }
  
  if (tws_con$server_version >= .server_version$MIN_SERVER_VER_AGG_GROUP) {
    contract$aggGroup <- as.integer(msg[count()])
  }
  
  if (tws_con$server_version >= .server_version$MIN_SERVER_VER_UNDERLYING_INFO) {
    contract$underSymbol <- msg[count()]
    contract$underSecType <- msg[count()]
  }
  
  if (tws_con$server_version >= .server_version$MIN_SERVER_VER_MARKET_RULES) {
    contract$marketRuleIds <- msg[count()]
  }
  
  if (tws_con$server_version >= .server_version$MIN_SERVER_VER_REAL_EXPIRATION_DATE) {
    contract$realExpirationDate <- msg[count()]
  }
  
  contract
}

#' @keywords internal
processContractDataEndMsg <- function(msg) {
  count <- counter()
  version <- as.integer(msg[count()])
  reqId <- as.integer(msg[count()])
  print(glue("next reqId is {reqId}"))
  # reqId
}



# readLastTradeDate <- function(fields, isBond = FALSE) {
#   lastTradeDateOrContractMonth <- fields
#   if (lastTradeDateOrContractMonth != NULL) {
#     splitted = strsplit(lastTradeDateOrContractMonth, split = "\\s+")
#   }
#   if length(splitted) > 1 {
#     if (isBond) {
#   contract$maturity = splitted[1]
#     } else {
#       contract$contract$lastTradeDateOrContractMonth = splitted[1]
#     }
#   }
#   
#    if (length(splitted) > 2) {
#     contract$lastTradeTime = splitted[1]
#    }
#   
#   if (isBond && length(splitted) > 3){
#     contract$timeZoneId = splitted[3]
#   }
# }