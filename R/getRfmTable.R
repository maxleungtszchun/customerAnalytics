#' Get RFM values and scores
#'
#' @param customerData tibble, customer unique records, must contain primary key.
#' @param transactionData tibble, transaction records, must contain foreign key.
#' @param id key name, should be found in both \code{customerData} and \code{transactionData}.
#' @param tData_visitDate field name of visit date column in \code{transactionData}. This field must be in the class of \code{POSIXct}. Any records containing \code{NA} will be removed.
#' @param tData_revenue field name of revenue column in \code{transactionData}, any records containing \code{NA} will be removed.
#' @param cData_visitDate field name of last visit date in \code{customerData}. This field must be in the class of \code{POSIXct};
#' optional, default value is \code{NULL}. If using default value, \code{recencyFloorDate} will be applied to fill \code{tData_visitDate}
#' for the cases of "did not come back" (customers were only found in \code{customerData}, but could not be found in \code{transactionData}).
#' If not using default value, the specified field will be applied to fill \code{tData_visitDate}
#' for the cases of "did not come back".
#' If the specified field contains missing values, highly recommend to use \code{recencyFloorDate} instead.
#' @param recencyFloorDate recency floor date for "did not come back" customers;
#' optional, default value is the oldest date of \code{tData_visitDate}, must be in the class of \code{POSIXct}.
#' @param reportDate recency ceiling date; optional, default value is the newest date of \code{tData_visitDate},
#' must be in the class of \code{POSIXct}.
#' @param bins bins for RFM scores; optional, default value is 5.
#' @return A tibble containing RFM values, percentile rank, scores and z-score.
#' @export

getRfmTable <- function (customerData, transactionData, id,
                         tData_visitDate, tData_revenue,
                         cData_visitDate = NULL,
                         recencyFloorDate = NULL, reportDate = NULL,
                         bins = 5) {

  id <- rlang::enquo(id)
  tData_visitDate <- rlang::enquo(tData_visitDate)
  tData_revenue <- rlang::enquo(tData_revenue)
  cData_visitDate <- rlang::enquo(cData_visitDate)

  noId <- is.na(customerData[[rlang::quo_name(id)]])
  if (any(noId)) {
    customerData %<>%
      dplyr::filter(!is.na(!! id))
    warning(paste0(sum(as.numeric(noId)), " records in `customerData` are deleted because of missing `id`."))
  }

  if (!(any(colnames(customerData) == rlang::quo_name(id)) &&
        any(colnames(transactionData) == rlang::quo_name(id)))) {
    stop("either `customerData` or `transactionData` or both missed keys")
  }

  noVisitDate <- is.na(transactionData[[rlang::quo_name(tData_visitDate)]])
  if (any(noVisitDate)) {
    transactionData %<>%
      dplyr::filter(!is.na(!! tData_visitDate))
    warning(paste0(sum(as.numeric(noVisitDate)), " records in `transactionData` are deleted because of missing `tData_visitDate`."))
  }

  noRevenue <- is.na(transactionData[[rlang::quo_name(tData_revenue)]])
  if (any(noRevenue)) {
    transactionData %<>%
      dplyr::filter(!is.na(!! tData_revenue))
    warning(paste0(sum(as.numeric(noRevenue)), " records in `transactionData` are deleted because of missing `tData_revenue`."))
  }

  countTransaction <-
    transactionData %>%
    dplyr::count(!! id)

  if (is.null(recencyFloorDate)) {
    recencyFloorDate <- transactionData %>%
      dplyr::pull(!! tData_visitDate) %>%
      min()
  }

  if (is.null(reportDate)) {
    reportDate <- transactionData %>%
      dplyr::pull(!! tData_visitDate) %>%
      max()
  }

  rfm <-
    customerData %>%
    dplyr::left_join(transactionData, by = rlang::quo_name(id)) %>%
    # using n() in summarize() will give wrong answer as "did not come back" will also be counted as 1
    # so left join another tibble which contains count values
    dplyr::left_join(countTransaction, by = rlang::quo_name(id)) %>%
    dplyr::group_by(!! id)

  if (is.null(rlang::quo_get_expr(cData_visitDate))) {

    rfm %<>%
      dplyr::summarize(recency = (reportDate - max(!! tData_visitDate)) / lubridate::ddays(),
                       # n are the same for all transaction records for each customer,
                       # so average will give the correct value (n x m / m = n)
                       # "did not come back" return NaN
                       frequency = mean(n, na.rm = TRUE),
                       monetary = sum(!! tData_revenue, na.rm = TRUE)) %>%
      fillRecency(TRUE, reportDate, recencyFloorDate)

  } else {

    rfm %<>%
      dplyr::summarize(recency = (reportDate - max(!! tData_visitDate)) / lubridate::ddays(),
                       cData_recency = (reportDate - max(!! cData_visitDate)) / lubridate::ddays(),
                       frequency = mean(n, na.rm = TRUE),
                       monetary = sum(!! tData_revenue, na.rm = TRUE)) %>%
      fillRecency(FALSE) %>% # no need to specify other arguments because of lazy evaluation
      dplyr::select(-cData_recency)

  }

  rfm %<>%
    fillFrequency() %>%
    dplyr::mutate(return = dplyr::if_else(frequency > 0, 1, 0)) %>%
    dplyr::mutate(negative_r_percentile = dplyr::percent_rank(-recency),
                  # negative receny is used here as lower recency will get higher score
                  # so higher negative recency (lower recency) will get higher score
                  # so higher percentile rank of negative recency will get higher score
                  # so that getScore() can be applied directly
                  f_percentile = dplyr::percent_rank(frequency),
                  m_percentile = dplyr::percent_rank(monetary),
                  negative_r_zScore = ((recency - mean(recency, na.rm = TRUE)) / sd(recency, na.rm = TRUE)) * -1,
                  f_zScore = (frequency - mean(frequency, na.rm = TRUE)) / sd(frequency, na.rm = TRUE),
                  m_zScore = (monetary - mean(monetary, na.rm = TRUE)) / sd(monetary, na.rm = TRUE)) %>%
    getScore("negative_r_percentile", "r_score", bins) %>%
    getScore("f_percentile", "f_score", bins) %>%
    getScore("m_percentile", "m_score", bins)

  return(rfm)

}
