#' Online Retail Data Set
#'
#' This is a transnational data set which contains all the transactions
#' occurring between 01/12/2010 and 09/12/2011 for a UK-based and registered
#' non-store online retail.
#'
#' Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
#'
#' @format A tibble with 541,909 rows and 8 variables:
#' \describe{
#'   \item{InvoiceNo}{chr. Invoice number. Nominal, a 6-digit integral number uniquely assigned to each transaction. If this code starts with letter 'c', it indicates a cancellation.}
#'   \item{StockCode}{chr. Product (item) code. Nominal, a 5-digit integral number uniquely assigned to each distinct product.}
#'   \item{Description}{chr. Product (item) name. Nominal.}
#'   \item{Quantity}{num. The quantities of each product (item) per transaction. Numeric.}
#'   \item{InvoiceDate}{POSIXct. Invoice Date and time. Numeric, the day and time when each transaction was generated.}
#'   \item{UnitPrice}{num. Unit price. Numeric, Product price per unit in sterling.}
#'   \item{CustomerID}{num. Customer number. Nominal, a 5-digit integral number uniquely assigned to each customer.}
#'   \item{Country}{chr. Country name. Nominal, the name of the country where each customer resides.}
#' }
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Online+Retail}
"transaction_data_all"

#' Customer List from 01/12/2010 to 31/05/2011
#'
#' This is a customer data set which includes all customers from 01/12/2010 to 31/05/2011.
#' Each record represents each unique customer.
#'
#' Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
#'
#' @format A tibble with 2,718 rows and 3 variables:
#' \describe{
#'   \item{CustomerID}{num. Customer number. Nominal, a 5-digit integral number uniquely assigned to each customer.}
#'   \item{visitDate}{POSIXct. last visit date of each customer during the period.}
#'   \item{Country}{chr. the latest (by invoice date during the period) country information of each customer}
#' }
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Online+Retail}
"customer_data_t0"

#' Customer List from 01/06/2011 to 30/11/2011
#'
#' This is a customer data set which includes all customers from 01/06/2011 to 30/11/2011.
#' Each record represents each unique customer.
#'
#' Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
#'
#' @format A tibble with 3,471 rows and 3 variables:
#' \describe{
#'   \item{CustomerID}{num. Customer number. Nominal, a 5-digit integral number uniquely assigned to each customer.}
#'   \item{visitDate}{POSIXct. last visit date of each customer during the period.}
#'   \item{Country}{chr. the latest (by invoice date during the period) country information of each customer}
#' }
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Online+Retail}
"customer_data_t1"

#' Transaction records from 01/12/2010 to 31/05/2011
#'
#' This is a transactional data set which includes all customers' transaction
#' from 01/12/2010 to 31/05/2011.
#' Each record represents each transaction of customers.
#'
#' Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
#'
#' @format A tibble with 145,450 rows and 9 variables:
#' \describe{
#'   \item{InvoiceNo}{chr. Invoice number. Nominal, a 6-digit integral number uniquely assigned to each transaction. If this code starts with letter 'c', it indicates a cancellation.}
#'   \item{StockCode}{chr. Product (item) code. Nominal, a 5-digit integral number uniquely assigned to each distinct product.}
#'   \item{Description}{chr. Product (item) name. Nominal.}
#'   \item{Quantity}{num. The quantities of each product (item) per transaction. Numeric.}
#'   \item{InvoiceDate}{POSIXct. Invoice Date and time. Numeric, the day and time when each transaction was generated.}
#'   \item{UnitPrice}{num. Unit price. Numeric, Product price per unit in sterling.}
#'   \item{CustomerID}{num. Customer number. Nominal, a 5-digit integral number uniquely assigned to each customer.}
#'   \item{Country}{chr. Country name. Nominal, the name of the country where each customer resides.}
#'   \item{revenue}{num. UnitPrice x Quantity}
#' }
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Online+Retail}
"transaction_data_t0"

#' Transaction records from 01/06/2011 to 30/11/2011
#'
#' This is a transactional data set which includes all customers' transaction
#' from 01/06/2011 to 30/11/2011.
#' Each record represents each transaction of customers.
#'
#' Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
#'
#' @format A tibble with 235,130 rows and 9 variables:
#' \describe{
#'   \item{InvoiceNo}{chr. Invoice number. Nominal, a 6-digit integral number uniquely assigned to each transaction. If this code starts with letter 'c', it indicates a cancellation.}
#'   \item{StockCode}{chr. Product (item) code. Nominal, a 5-digit integral number uniquely assigned to each distinct product.}
#'   \item{Description}{chr. Product (item) name. Nominal.}
#'   \item{Quantity}{num. The quantities of each product (item) per transaction. Numeric.}
#'   \item{InvoiceDate}{POSIXct. Invoice Date and time. Numeric, the day and time when each transaction was generated.}
#'   \item{UnitPrice}{num. Unit price. Numeric, Product price per unit in sterling.}
#'   \item{CustomerID}{num. Customer number. Nominal, a 5-digit integral number uniquely assigned to each customer.}
#'   \item{Country}{chr. Country name. Nominal, the name of the country where each customer resides.}
#'   \item{revenue}{num. UnitPrice x Quantity}
#' }
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Online+Retail}
"transaction_data_t1"



