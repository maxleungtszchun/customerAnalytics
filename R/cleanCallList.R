toChar <- function (df, field) {
  toupper(trimws(as.character(df[ ,field])))
}

esToNa <- function (df, field) {

  df[which(df[ ,field] == ""),field] <- NA

  return(df[ ,field])

}

changeText <- function (df, fields, original, replace, fixed = FALSE) {

  rows <- nrow(df)

  df[fields] <- vapply(fields, toChar, df = df, FUN.VALUE = character(rows))
  df[fields] <- vapply(df[fields], gsub, pattern = original, replacement = replace, fixed = fixed,
                                            FUN.VALUE = character(rows))
  df[fields] <- vapply(fields, esToNa, df = df, FUN.VALUE = character(rows))

  return(df)

}

rowTrue <- function (criteria, fun) {

  if (!is.null(criteria) && is.atomic(criteria) && !is.matrix(criteria)) {

    apply(t(as.matrix(criteria)), 1, fun)

  } else if (is.matrix(criteria)) {

    apply(criteria, 1, fun)

  } else {

    stop("input of rowTrue must be an atomic vector or a matrix")

  }

}

ncharM <- function (df) {
  vapply(df, nchar, FUN.VALUE = integer(nrow(df)))
}

substringM <- function (df, first, last) {
  vapply(df, substring, first, last, FUN.VALUE = character(nrow(df)))
}

zeroOrOneM <- function (df) {
  substringM(df, 1, 1) == "0" | substringM(df, 1, 1) == "1"
}

checkFunctionFactory <- function (criteria) {

  function (df, fields) {

    df[fields] <- vapply(fields, toChar, df = df, FUN.VALUE = character(nrow(df)))

    keep <- eval(parse(text = criteria))
    if (length(keep) > 0) df <- df[keep, ] else stop("all records are wrong.")

    return(df)

  }

}

checkLen <- checkFunctionFactory("which(!(rowTrue(ncharM(df[fields]) != 8, all)))")
checkDigit <- checkFunctionFactory("which(!(rowTrue(zeroOrOneM(df[fields]), all)))")

checkValueDup <- function (df, checkFields, value) {

  if (!is.na(value)) {

    return(expss::count_if(value, df[checkFields]))

  } else {

    return(NA)

  }

}

checkWithinDup <- function (row) {

  row[duplicated(row)] <- NA

  return(row)

}

withinDup <- function (df, fields) {

  df[fields] <- vapply(fields, toChar, df = df, FUN.VALUE = character(nrow(df)))

  df[fields] <- t(apply(df[fields], 1, checkWithinDup))

  return(df)

}

betweenDup <- function (df, fields) {

  df[fields] <- vapply(fields, toChar, df = df, FUN.VALUE = character(nrow(df)))

  for (field in fields) {
    df[paste0(field, ".dup")] <- vapply(df[ ,field], checkValueDup, df = df, checkFields = fields,
                                        FUN.VALUE = integer(1))
  }

  dupNames <- colnames(df)[endsWith(colnames(df), ".dup")]

  df[which(rowTrue(df[ ,dupNames] > 1, any)),"check"] <- "dup"

  if (length(which(df$check == "dup")) > 0) {
    df <- df[-which(df$check == "dup"), ]
  }

  return(df)

}

#' Remove invalid phone numbers in multiple columns
#'
#' Invalid phone numbers in a specific record:
#' \enumerate{
#'   \item all phone numbers that are not equal to 8 digits, including no phone numbers at all (all zero digit).
#'   \item all phone numbers that start from number zero or one.
#'   \item all phone numbers that duplicate between other records, excluding phone numbers duplicate within the same record.
#' }
#'
#' @param df data frame containing at least two rows and two columns of phone numbers.
#' @param field character vector containing the field names of phone numbers in \code{df}.
#' @param details optional, default value is \code{all} which returns extra information. \code{onlyFields} omits such information.
#' @return data frame
#' @export

removeDup <- function (df, fields, details = "all") {

  if (nrow(df[fields]) <= 1) {

    stop("removeDup requires at least two rows in data frame.")

  } else if (length(fields) <= 1) {

    stop("removeDup requires at least two fields of phone number.")

  } else {

    df <- changeText(df, fields, "[^0-9]", "")
    df <- checkLen(df, fields)
    df <- checkDigit(df, fields)
    df <- withinDup(df, fields)
    df <- betweenDup(df, fields)

    switch(details,
           all = return(df),
           onlyFields = return(df[fields]),
           stop("only `all` or `onlyFields` is allowed."))

  }

}

countUnique <- function (row) {
  length(unique(row))
}

repeatName <- function (df, fields) {

  df[fields] <- vapply(fields, toChar, df = df, FUN.VALUE = character(nrow(df)))

  df["repeat_name"] <- apply(df[fields], 1, countUnique)
  df[which(df$repeat_name == 1),fields[2:length(fields)]] <- NA

  return(df[ ,-match("repeat_name", colnames(df))])

}

regexprNA <- function (pattern, text) {

  if (!is.na(pattern)) {

    return(regexpr(pattern = pattern, text = text))

  } else {

    return(rep(NA, length(text)))

  }

}

matchSurname <- function (df, fields, i) {

  if (!is.data.frame(df[ ,fields[-i]])) {

    surIndex <- mapply(regexprNA, pattern = stringr::str_c("\\<", df[ ,fields[i]], "\\>"), text = df[ ,fields[-i]])

  } else {

    surIndex <- t(mapply(regexprNA, pattern = stringr::str_c("\\<", df[ ,fields[i]], "\\>"), text = split(df[fields[-i]], seq(nrow(df[fields[-i]])))))

  }
  dim(surIndex) <- c(nrow(df[fields]), length(fields) - 1)

  matchSurIndex <- rowTrue(surIndex == 1 &
                             df[ ,fields[i]] == substringM(df[fields[-i]], 1, nchar(df[ ,fields[i]])),
                           any)
  df[which(matchSurIndex),fields[i]] <- NA

  return(df)

}

dupSurname <- function (df, fields) {

  df[fields] <- vapply(fields, toChar, df = df, FUN.VALUE = character(nrow(df)))

  for (i in seq_along(fields)) {
    df <- matchSurname(df, fields, i)
  }

  return(df)

}

delNoName <- function (df, fields) {

  df[fields] <- vapply(fields, toChar, df = df, FUN.VALUE = character(nrow(df)))

  noNameIndex <- apply(df[fields], 1, function(row) all(is.na(row)))

  if (length(which(noNameIndex)) > 0) {
    df <- df[-which(noNameIndex), ]
  }

  return(df)

}

#' Remove invalid names in multiple columns
#'
#' Invalid names in a specific record:
#' \enumerate{
#'   \item no names at all.
#' }
#'
#' @param df data frame containing at least two rows and two columns of names.
#' @param field character vector containing the field names of names in \code{df}.
#' @return data frame
#' @export

cleanName <- function (df, fields) {

  if (nrow(df[fields]) <= 1) {

    stop("cleanName requires at least two rows in data frame.")

  } else if (length(fields) <= 1) {

    stop("cleanName requires at least two fields of name.")

  } else {

    df <- changeText(df, fields, "[^A-Z&' '&\U4E00-\U9FFF\U3000-\U303F]", "")
    df <- changeText(df, fields, "\\<NIL\\>", "")
    df <- repeatName(df, fields)
    df <- dupSurname(df, fields)
    df <- delNoName(df, fields)

    return(df)

  }

}
