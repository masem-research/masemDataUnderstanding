# Function: show numbered list of variable names in a dataset -------------


#' PrintDisplay numbered list of variables in a dataset
#'
#' @description Often it is really useful to have a numbered list of variables
#' in a dataset, especially if the dataset is new. The function will provide such
#' a list
#'
#' @param data.frame. Input object: data.frame.
#'
#' @return data.frame. data.frame with row numbers corresponding to the position of the
#' variables in the dataset and variable names.
#' @export
#'
#' @examples
#' # iris
#' numberedVariablesList(df = iris)
#' # mtcars
#' numberedVariablesList(df = mtcars)
numberedVariablesList <- function(df) {
  if (!is.data.frame(df)) {
    warning("input object is not a data.frame!")
    return(NA_real_)
  }
  return(data.frame(variableNames = names(df)))
}




#' Frequency table with number of cases and relative number of cases (%)
#'
#' @description Provides a table with absolute and relative number of cases.
#' The output table can be easily used in Markdown with kable().
#' @param x vector. Should be a vector.
#'
#' @return data.frame. absolute and relative values
#' @export
#'
#' @examples
#' ## iris
#' freqTable(x = iris$Species)
#' ## mtcars
#' # automatic or manual?
#' freqTable(x = mtcars$am)
#' # number of gears
#' freqTable(x = mtcars$gear)
freqTable <- function(x) {
  # generate absolute and relative frequencies
  table.absolute.values <- table(x)
  table.relative.values <- prop.table(table.absolute.values)*100
  # combine both variables
  df.absolute.and.relative.values <- data.frame(table.absolute.values)
  df.absolute.and.relative.values$p <- table.relative.values
  # set column names
  names(df.absolute.and.relative.values) <- c("value","n","p [%]")
  # return df
  return(df.absolute.and.relative.values)
}






