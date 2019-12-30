#' Plot Heat Map of RFM
#'
#' @param df data frame or tibble.
#' @param r_score field name of recency score in \code{df}.
#' @param f_score field name of frequency score in \code{df}.
#' @param m_value field name of monetary value in \code{df}.
#' @param rfm_score field name of weighted RFM score in \code{df}.
#' @param low color for low value of \code{m_value}; default value is \code{cornsilk}.
#' @param high color for high value of \code{m_value}; default value is \code{orange}.
#' @param size the size of annotation; optional; default value is 4.
#' @param ... extra arguments go to \code{geom_tile()}.
#' @return a ggplot.
#' @export

plotHeatMap <- function (df, r_score, f_score, m_value, rfm_score,
                         low = "cornsilk", high = "orange", size = 4, ...) {

  r_score <- rlang::enquo(r_score)
  f_score <- rlang::enquo(f_score)
  m_value <- rlang::enquo(m_value)
  rfm_score <- rlang::enquo(rfm_score)

  df %>%
    dplyr::group_by(!! r_score, !! f_score) %>%
    dplyr::summarize(n = dplyr::n(), mean_monetary = mean(!! m_value, na.rm = TRUE),
                     mean_rfm_score = mean(!! rfm_score, na.rm = TRUE)) %>%
    ggplot2::ggplot(ggplot2::aes(x = !! r_score, y = !! f_score)) +
    ggplot2::geom_tile(ggplot2::aes(fill = mean_monetary), colour = "White", ...) +
    ggplot2::scale_fill_gradient(low = low, high = high) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("RFM score = ", round2(mean_rfm_score, 2))),
                       vjust = -0.5, size = size) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("n = ", n)), vjust = 1, size = size) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::labs(x = "Recency Score", y = "Frequency Score", title = "RFM Heat Map")

}
