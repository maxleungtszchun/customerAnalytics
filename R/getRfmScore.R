#' Get RFM score
#'
#' @param df tibble.
#' @param r_score field name of recency score in \code{df}.
#' @param f_score field name of frequency score in \code{df}.
#' @param m_score field name of monetary score in \code{df}.
#' @param weights a numeric vector containing RFM weights, must have names.
#' @param r_weight name of recency weight in \code{weights}.
#' @param f_weight name of frequency weight in \code{weights}.
#' @param m_weight name of monetary weight in \code{weights}.
#' @return an original tibble with an extra column of weighted RFM scores.
#' @export

getRfmScore <- function (df, r_score, f_score, m_score,
                         weights, r_weight, f_weight, m_weight) {

  r_score <- rlang::enquo(r_score)
  f_score <- rlang::enquo(f_score)
  m_score <- rlang::enquo(m_score)

  df %<>%
    dplyr::mutate(rfm_score = !! r_score * weights[[r_weight]] +
                    !! f_score * weights[[f_weight]] +
                    !! m_score * weights[[m_weight]])

  return(df)

}
