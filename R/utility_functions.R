#' @importFrom magrittr %>% %<>%

fillRecency <- function (df, useRecencyFloor, reportDate, recencyFloorDate) {

  fillRows <- is.na(df[["recency"]])

  if (useRecencyFloor == TRUE) {
    df[fillRows,"recency"] <- (reportDate - recencyFloorDate) / lubridate::ddays()
  } else {
    df[fillRows,"recency"] <- df[fillRows,"cData_recency", drop = TRUE]
  }

  return(df)

}

fillFrequency <- function (df) {

  fillRows <- is.na(df[["frequency"]])
  df[fillRows,"frequency"] <- 0

  return(df)

}

getScore <- function (df, percentileField, scoreField, bins) {

  percent <- 1 / bins

  for (i in seq_len(bins)) {

    if (i == 1) {
      df[df[[percentileField]] <= percent * i, scoreField] <- i
    } else if (i == bins) {
      df[df[[percentileField]] > percent * (i - 1), scoreField] <- i
    } else {
      df[df[[percentileField]] > percent * (i - 1) &
           df[[percentileField]] <= percent * i, scoreField] <- i
    }

  }

  return(df)

}

#rounding function found in
#https://stackoverflow.com/questions/12688717/round-up-from-5
round2 <- function(x, n) (trunc((abs(x) * 10 ^ n) + 0.5) / 10 ^ n) * sign(x)
