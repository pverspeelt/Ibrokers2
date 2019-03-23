#' Function for filling the contract details
#' 
#' This function is used for filling the contract details and is filled when
#' using the function req_contract_details.
#'
#' @param contract The contract information. Detailed information can be found 
#' in the \code{\link{contract}} function
#' @param marketName The market name for this product. 
#' @param minTick The minimum allowed price variation. Note that many securities
#'  vary their minimum tick size according to their price. This value will only 
#'  show the smallest of the different minimum tick sizes regardless of the 
#'  product's price. Full information about the minimum increment price structure 
#'  can be obtained with the reqMarketRule function or the IB Contract 
#'  and Security Search site.
#' @param orderTypes Supported order types for this product. 
#' @param validExchanges Valid exchange fields when placing an order for this contract.
#' @param priceMagnifier Allows execution and strike prices to be reported 
#' consistently with market data, historical data and the order price, 
#' i.e. Z on LIFFE is reported in Index points and not GBP.
#' @param underConId For derivatives, the contract ID (conID) of the underlying instrument. 
#' @param longName Descriptive name of the product.
#' @param contractMonth Typically the contract month of the underlying for a Future contract. 
#' @param industry The industry classification of the underlying/product. For example, Financial. 
#' @param category The industry category of the underlying. For example, InvestmentSvc. 
#' @param subcategory The industry subcategory of the underlying. For example, Brokerage. 
#' @param timeZoneId The time zone for the trading hours of the product. For example, EST. 
#' @param tradingHours The trading hours of the product. This value will 
#' contain the trading hours of the current day as well as the next's.
#' @param liquidHours The liquid hours of the product. This value will contain 
#' the liquid hours (regular trading hours) of the contract on the specified exchange. 
#' @param evRule Contains the Economic Value Rule name and the respective optional 
#' argument. The two values should be separated by a colon. For example, 
#' aussieBond:YearsToExpiration=3. When the optional argument is not present, 
#' the first value will be followed by a colon. 
#' @param evMultiplier Tells you approximately how much the market value of a 
#' contract would change if the price were to change by 1. It cannot be used to 
#' get market value by multiplying the price by the approximate multiplier. 
#' @param mdSizeMultiplier MD Size Multiplier. Returns the size multiplier for 
#' values returned to tickSize from a market data request. 
#' Generally 100 for US stocks and 1 for other instruments. 
#' @param aggGroup Aggregated group Indicates the smart-routing group to which 
#' a contract belongs. contracts which cannot be smart-routed have aggGroup = -1. 
#' @param underSymbol For derivatives, the symbol of the underlying contract. 
#' @param underSecType For derivatives, returns the underlying security type. 
#' @param marketRuleIds The list of market rule IDs separated by comma.
#' @param secIdList A list of contract identifiers that the customer is allowed to view. 
#' @param realExpirationDate Real expiration date.
#' @param lastTradeTime Last trade time. 
#' @param cusip The nine-character bond CUSIP. For Bonds only. 
#' Receiving CUSIPs requires a CUSIP market data subscription. 
#' @param ratings Identifies the credit rating of the issuer. 
#' This field is not currently available from the TWS API.
#' @param descAppend A description string containing further descriptive 
#' information about the bond. For Bonds only.
#' @param bondType The type of bond, such as "CORP.".
#' @param couponType The type of bond coupon. This field is currently not 
#' available from the TWS API. For Bonds only. 
#' @param callable If true, the bond can be called by the issuer under certain 
#' conditions. This field is currently not available from the TWS API. For Bonds only.
#' @param putable Values are True or False. If true, the bond can be sold back 
#' to the issuer under certain conditions. This field is currently not available
#'  from the TWS API. For Bonds only. 
#' @param coupon The interest rate used to calculate the amount you will receive
#'  in interest payments over the course of the year. 
#'  This field is currently not available from the TWS API. For Bonds only. 
#' @param convertible Values are True or False. If true, the bond can be 
#' converted to stock under certain conditions. This field is currently not 
#' available from the TWS API. For Bonds only.
#' @param maturity The date on which the issuer must repay the face value of 
#' the bond. This field is currently not available from the TWS API. For Bonds only.
#' @param issueDate The date the bond was issued. This field is currently not 
#' available from the TWS API. For Bonds only.
#' @param nextOptionDate Only if bond has embedded options. This field is 
#' currently not available from the TWS API.
#' @param nextOptionType Type of embedded option. This field is currently not 
#' available from the TWS API.
#' @param nextOptionPartial Only if bond has embedded options. This field is 
#' currently not available from the TWS API. For Bonds only. 
#' @param notes If populated for the bond in IB's database. For Bonds only. 
#'
#' @return An object of class contract_details
#' @export
#'
#' @examples
#' Example to come
contractDetails <- function(contract = contract(), 
                            marketName = "",
                            minTick = 0,
                            orderTypes = "",
                            validExchanges = "",
                            priceMagnifier = 0,
                            underConId = 0,
                            longName = "",
                            contractMonth = "",
                            industry = "",
                            category = "",
                            subcategory = "",
                            timeZoneId = "",
                            tradingHours = "",
                            liquidHours = "",
                            evRule = "",
                            evMultiplier = 0,
                            mdSizeMultiplier = 0,
                            aggGroup = 0,
                            underSymbol = "",
                            underSecType = "",
                            marketRuleIds = "",
                            secIdList = NULL,
                            realExpirationDate = "",
                            lastTradeTime = "",
                            # BOND values
                            cusip = "",
                            ratings = "",
                            descAppend = "",
                            bondType = "",
                            couponType = "",
                            callable = FALSE,
                            putable = FALSE,
                            coupon = 0,
                            convertible = FALSE,
                            maturity = "",
                            issueDate = "",
                            nextOptionDate = "",
                            nextOptionType = "",
                            nextOptionPartial = FALSE,
                            notes = "") {
  structure(list(
    contract = contract,
    marketName = marketName,
    minTick = minTick,
    orderTypes = orderTypes,
    validExchanges = validExchanges,
    priceMagnifier = priceMagnifier,
    underConId = underConId,
    longName = longName,
    contractMonth = contractMonth,
    industry = industry,
    category = category,
    subcategory = subcategory,
    timeZoneId = timeZoneId,
    tradingHours = tradingHours,
    liquidHours = liquidHours,
    evRule = evRule,
    evMultiplier = evMultiplier,
    mdSizeMultiplier = mdSizeMultiplier,
    aggGroup = aggGroup,
    underSymbol = underSymbol,
    underSecType = underSecType,
    marketRuleIds = marketRuleIds,
    secIdList = secIdList,
    realExpirationDate = realExpirationDate,
    lastTradeTime = lastTradeTime,
    # BOND values
    cusip = cusip,
    ratings = ratings,
    descAppend = descAppend,
    bondType = bondType,
    couponType = couponType,
    callable = callable,
    putable = putable,
    coupon = coupon,
    convertible = convertible,
    maturity = maturity,
    issueDate = issueDate,
    nextOptionDate = nextOptionDate,
    nextOptionType = nextOptionType,
    nextOptionPartial = nextOptionPartial,
    notes = notes
  ), 
  class="contract_details")
}
