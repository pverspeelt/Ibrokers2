% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/req_managed_accounts.R
\name{req_managed_accounts}
\alias{req_managed_accounts}
\title{Request managed accounts list}
\usage{
req_managed_accounts(tws_con)
}
\arguments{
\item{tws_con}{a valid tws_con object.}
}
\value{
prints the managed accounts that a TWS user name handles when connected to TWS.
}
\description{
Important: whenever your TWS user name handles more than a single account, 
you will be forced to specify the account Id to which the order needs 
to be allocated. Failure to do so will result in the order being rejected 
since the TWS cannot assign the order to a default account.
}
\examples{
example here
}
