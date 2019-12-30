#' Plot Bar Chart by Group
#'
#' @param df data frame or tibble.
#' @param groupVar field name for groupping in \code{df}.
#' @param measureVar field name of the measure in \code{df}.
#' @param title chart's title.
#' @param label label for \code{measureVar}.
#' @param fun aggregate function; optional, default value is \code{mean()}.
#' @param dp number of decimal places; optional, default value is 1.
#' @param mean_line show line of mean value; optional; default value is \code{TRUE}.
#' @param ... extra arguments go to \code{geom_bar()}.
#' @return a ggplot
#' @export

plotGroupBars <- function (df, groupVar, measureVar, title, label,
                           fun = mean, dp = 1, mean_line = TRUE, ...) {

  groupVar <- rlang::enquo(groupVar)
  measureVar <- rlang::enquo(measureVar)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(!! groupVar, !! measureVar,
                                                    function(x) fun(x, na.rm = TRUE)),
                                        y = !! measureVar)) +
    ggplot2::geom_bar(stat = "summary", fun.y = fun, na.rm = TRUE, ...) +
    ggplot2::stat_summary(ggplot2::aes(label = format(round2(..y.., dp),
                                                      big.mark = ",",
                                                      scientific = FALSE)),
                          fun.y = fun, na.rm = TRUE, geom = "text", hjust = -0.05) +
    ggplot2::labs(x = "", y = label, title = title) +
    ggplot2::scale_y_continuous(position = "right",
                                labels = function (x) format(round2(x, dp), big.mark = ",", scientific = FALSE)) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::theme_minimal() +
    EnvStats::stat_n_text(y.pos = 0, size = 3) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0, 0, 0, 0)))

  if (mean_line == TRUE) {
    p + ggplot2::geom_hline(ggplot2::aes(yintercept = mean(!! measureVar, na.rm = TRUE)))
  } else {
    p
  }

}
