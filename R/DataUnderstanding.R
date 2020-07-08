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




#' Cross table with row precentages
#'
#' @param column vector. Column Vector: Should be target variable
#' @param row vector. Row Vector
#'
#' @return table. Crosstable
#' @export
#'
#' @examples
#' # mtcars
#' crossTableRowPercentages(column = mtcars$am, row = mtcars$cyl)
crossTableRowPercentages <- function(column, row) {
  # note: y should be the target variable!
  options(digits = 5)
  cross.table.absolute.values <- table(row, column)
  cross.table.relative.values <- prop.table(cross.table.absolute.values, 1)*100
  return(cross.table.relative.values)
}




#' Display the target binary variable
#'
#' @description Display a binary (target) variable in ggplot2.
#'
#' @param DataFrame data.frame.
#' @param targetVariable. character. Name of (target) variable to display
#'
#' @return
#' @export
#'
#' @examples
#' ## Call
#' AbsoluteValues(DataFrame = mtcars, targetVariable = "am")
AbsoluteValues <- function(DataFrame, targetVariable) {
  ## TODO: (please check todor!)
  ## Add more colors for non-binary target variables
  ## Add a (corporate design) color vector
  ## ...
  ggplot(data = DataFrame, aes(x = as.factor(DataFrame[,targetVariable]),
                           fill = as.factor(DataFrame[,targetVariable]))) +
    geom_bar(aes(fill = as.factor(DataFrame[,targetVariable]))) +
    #scale_fill_manual(values = c("green", "red"),
    #                  labels = c("No", "Yes"),
    #                  name = "Target Variable") +
    theme(legend.position = "none") +
    labs(y = "Anzahl", x = "TargetVariable")
}




#' Displays a feature variable by a binary target variable
#'
#' @param DataFrame data.frame. data.frame under investigation.
#' @param Feature vector. Feature variable.
#' @param TargetVariable factor. Binary target variable.
#'
#' @return
#' @export
#'
#' @examples
#' ## mtcars
#' FeatureByTargetVariable(DataFrame = mtcars, Feature = "gear", TargetVariable = "am")
#' ## Selected variables in data.frame:
#' Variables <- c("cyl", "gear", "vs")
#' for (i in Variables) {
#'     print(FeatureByTargetVariable(DataFrame = mtcars, Feature = i, TargetVariable = "am"))
#' }
FeatureByTargetVariable <- function(DataFrame, Feature, TargetVariable) {
  ggplot(data = DataFrame, aes(DataFrame[,Feature], fill = as.factor(DataFrame[,TargetVariable]))) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c("green", "red"),
                      #labels = c("No", "Yes"),
                      name = "Target Variable") +
    theme(legend.position = "none") +
    labs(y = "count", x = paste("Feature:", Feature))
}





#' Numerical Feature by categorical target variable
#'
#' @param DataFrame. data.frame. data.frame under investigation.
#' @param NumericalFeature character. Numerical feature
#' @param TargetVariable character. Categorical target variable.
#'
#' @return chart.
#' @export
#'
#' @examples
#' NumericalFeatureByTargetVariable(DataFrame = iris, NumericalFeature = "Sepal.Length", TargetVariable = "Species")
NumericalFeatureByTargetVariable <- function(DataFrame, NumericalFeature, TargetVariable) {
  ggplot(data = DataFrame, aes(x = DataFrame[, NumericalFeature], fill = factor(DataFrame[, TargetVariable]))) +
    geom_density(alpha = .2) +
    theme(legend.position = "bottom")
}






