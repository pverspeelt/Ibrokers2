% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/req_contract_details.R
\name{req_contract_details}
\alias{req_contract_details}
\title{Request contract details}
\usage{
req_contract_details(contract, reqId = 1L, tws_con)
}
\arguments{
\item{contract}{a valid contract.}

\item{reqId}{the request's unique identifier. Should be a scalar integer.}

\item{tws_con}{a valid tws connection.}
}
\value{
a list of details for the specified contract.
}
\description{
Request contract details
}
\examples{
\dontrun{
tws_con <- tws_connect()
ibm <- contract(symbol = "IBM", 
                secType = "STK", 
                currency = "USD", 
                exchange = "SMART")
req_contract_details(ibm, tws_con)
}
}
