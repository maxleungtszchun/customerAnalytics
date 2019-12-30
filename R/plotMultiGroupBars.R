#' Plot Bar Chart by Group Over Time
#'
#' @param df data frame or tibble.
#' @param groupVar field name for groupping in \code{df}.
#' @param measureVar field name of the measure in \code{df}.
#' @param periodVar field name of the period in \code{df}.
#' @param sortFirst value in \code{periodVar}. sort the bars with this value first.
#' @param title chart's title.
#' @param label label for \code{measureVar}.
#' @param lagendTitle title of the legend.
#' @param fun aggregate function; optional, default value is \code{mean()}.
#' @param dp number of decimal places; optional, default value is 1.
#' @param colorVector character vector for setting the color of the bars; optional, default value is \code{c("gold", "orange")}.
#' @param ... extra arguments go to \code{geom_bar()}.
#' @return a ggplot
#' @export

plotPeriodGroupBars <- function (df, groupVar, measureVar, periodVar, sortFirst,
                                 title, label, legendTitle, fun = mean, dp = 1,
                                 colorVector = c("gold", "orange"), ...) {

  groupVar <- rlang::enquo(groupVar)
  measureVar <- rlang::enquo(measureVar)
  periodVar <- rlang::enquo(periodVar)

  order <-
    df %>%
    dplyr::filter(!! periodVar == sortFirst) %>%
    dplyr::group_by(!! groupVar) %>%
    dplyr::summarize(meanValue = mean(!! measureVar, na.rm = TRUE)) %>%
    dplyr::arrange(meanValue) %>%
    dplyr::pull(!! groupVar)

  df[[rlang::quo_name(groupVar)]] <- factor(df[[rlang::quo_name(groupVar)]], levels = order)

  ggplot2::ggplot(df, ggplot2::aes(x = !! groupVar,
                                   y = !! measureVar,
                                   fill = !! periodVar)) +
    ggplot2::geom_bar(position = "dodge", stat = "summary", fun.y = fun, na.rm = TRUE, ...) +
    ggplot2::scale_fill_manual(values = colorVector) +
    ggplot2::labs(x = "", y = label, title = title, fill = legendTitle) +
    ggplot2::scale_y_continuous(position = "right",
                                labels = function (x) format(round2(x, dp), big.mark = ",", scientific = FALSE)) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0, 0, 0, 0)),
                   legend.position  = "top") +
    ggplot2::stat_summary(ggplot2::aes(label = format(round2(..y.., dp),
                                                      big.mark = ",",
                                                      scientific = FALSE)),
                          fun.y = fun, na.rm = TRUE, geom = "text",
                          position = ggplot2::position_dodge(width = 0.85), hjust = -0.05) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, reverse = TRUE))

}
