#' Create a contract
#'
#' @param conId The unique IB contract identifier
#' @param symbol The contract (or its underlying) symbol.
#' @param secType The security type:
#' @param lastTradeDateOrContractMonth The contract's last trading day or 
#' contract month (for Options and Futures). 
#' Strings with format YYYYMM will be interpreted as the Contract Month 
#' whereas YYYYMMDD will be interpreted as Last Trading Day.
#' @param strike The option's strike price.
#' @param right Put or Call.
#' Valid values are 'P', 'PUT', 'C', 'CALL', or '' for non-options.
#' @param multiplier The instrument's multiplier (i.e. options, futures).
#' @param exchange The destination exchange.
#' @param primaryExchange The contract's primary exchange. For 
#' smart routed contracts, used to define contract in case of ambiguity. 
#' Should be defined as native exchange of contract, e.g. ISLAND for MSFT. 
#' For exchanges which contain a period in name, will only be part of 
#' exchange name prior to period, i.e. ENEXT for ENEXT.BE.
#' @param currency The underlying's currency.
#' @param localSymbol The contract's symbol within its primary exchange.
#' For options, this will be the OCC symbol.
#' @param tradingClass The trading class name for this contract.
#' Available in TWS contract description window as well. 
#' @param includeExpired If set to true, contract details requests and 
#' historical data queries can be performed pertaining to expired 
#' futures contracts. Expired options or other instrument types are not available.
#' @param secIdType Security identifier type
#' @param secId Security identifier.
#' @param comboLegsDescrip Description of the combo legs.
#' @param comboLegs The legs of a combined contract definition.
#' @param deltaNeutralContract Delta and underlying price for Delta-Neutral 
#' combo orders.
#'
#' @return returns an object of class "contract"
#' @export
#'
#' @examples
#' ibm <- contract(symbol = "IBM", 
#'                 secType = "STK", 
#'                 currency = "USD", 
#'                 exchange = "SMART")
contract <- function(conId = 0L,
                     symbol = "",
                     secType = "",
                     lastTradeDateOrContractMonth = "",
                     strike = 0,  # numeric!
                     right = "",
                     multiplier = "",
                     exchange = "",
                     primaryExchange = "", # pick an actual (ie non-aggregate) exchange that the contract trades on. DO NOT SET TO SMART.
                     currency = "",
                     localSymbol = "",
                     tradingClass = "",
                     includeExpired = FALSE,
                     secIdType = "",	  # CUSIP;SEDOL;ISIN;RIC
                     secId = "",
                     
                     #combos
                     comboLegsDescrip = "",  # type: str; received in open order 14 and up for all combos
                     comboLegs = NULL,     # type: list<ComboLeg>
                     deltaNeutralContract = NULL)
{
  structure(
    list(conId = conId,
         symbol = symbol,
         secType = secType,
         lastTradeDateOrContractMonth = lastTradeDateOrContractMonth,
         strike = strike,  # numeric!
         right = right,
         multiplier = multiplier,
         exchange = exchange,
         primaryExchange = primaryExchange, # pick an actual (ie non-aggregate) exchange that the contract trades on. DO NOT SET TO SMART.
         currency = currency,
         localSymbol = localSymbol,
         tradingClass = tradingClass,
         includeExpired = includeExpired,
         secIdType = secIdType,	  # CUSIP;SEDOL;ISIN;RIC
         secId = secId,
         
         #combos
         comboLegsDescrip = comboLegsDescrip,  # type: str; received in open order 14 and up for all combos
         comboLegs = comboLegs,     # type: list<ComboLeg>
         deltaNeutralContract = deltaNeutralContract
    ),
    class="contract"
  )
  
}