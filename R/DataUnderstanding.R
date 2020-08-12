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
    #warning("input object is not a data.frame!")
    #return(NA)
    stop("input object is not a data.frame!")
  }
  return(data.frame(variableNames = names(df)))
}





#' Displays Metadata of variables in a data.frame
#'
#' @param DataFrame data.frame. data to be analyzed. data.frame required.
#'
#' @details
#' Returns four columns in a data.frame:
#' 1: VarNo:         Running Number
#' 2: VarNames:      Variable Name
#' 3: StorageType:   Atomic class
#' 4: UniqueValues:  Number of unique categories
#'
#' @return data.frame. data.frame with metadata.
#' @export
#'
#' @examples
#' ## famous iris data set
#' VariableMetaData(DataFrame = iris)
#' ## mtcars data set
#' VariableMetaData(DataFrame = mtcars)
VariableMetaData <- function(DataFrame) {
  ## Returns four columns in a data.frame:
  #   1: VarNo:         Running Number
  #   2: VarNames:      Variable Name
  #   3: StorageType:   Atomic class
  #   4: UniqueValues:  Number of unique categories
  ## Generate data.frame
  VariableMetaData <- data.frame(VarNo = 1: length(names(DataFrame)),
                                 VarNames = names(DataFrame),
                                 StorageType = sapply(DataFrame, class),
                                 UniqueValues = sapply(DataFrame, function(x) length(unique(x))))
  ## Delete row,names
  rownames(VariableMetaData) <- NULL
  ## Return
  return(VariableMetaData)
  }




#' Frequency table with number of cases and relative number of cases
#'
#' @description Provides a table with absolute and relative number of cases.
#' The output table can be easily used in Markdown with kable().
#'
#' @details Per default NA values will be displayed, no matter if there are any.
#' Set `printNA` argument to FALSE to surpress NA values. Please note: If there are
#' any NA values in x (variable), a warning will be issued!
#'
#' @param x vector. Should be a vector.
#' @param printNA boolean. Print NA values? Default is TRUE
#'
#' @return data.frame. absolute and relative values
#' @export
#'
#' @examples
#' ## iris
#' freqTable(x = iris$Species, printNA = TRUE)
#' ## mtcars
#' # automatic or manual?
#' freqTable(x = mtcars$am, printNA = FALSE)
#' # number of gears
#' freqTable(x = mtcars$gear, printNA = FALSE)
#' # NA values in variable
#' freqTable(x = airquality$Ozone, printNA = TRUE)
#' freqTable(x = airquality$Ozone, printNA = FALSE)
freqTable <- function(x, printNA = TRUE) {
  ## set NA parameter in table-function argument useNA:
  printNATable <- "always"
  if (!printNA) {
    printNATable <- "no"
  }
  ## Print warning if NAs values are in vector!
  if (sum(is.na(x)) > 0) {
    warning("NAs in vector!")
  }
  ## generate absolute and relative frequencies
  table.absolute.values <- table(x, useNA = printNATable)
  table.relative.values <- prop.table(table.absolute.values)*100
  ## combine both variables
  df.absolute.and.relative.values <- data.frame(table.absolute.values)
  df.absolute.and.relative.values$p <- table.relative.values
  ## set column names
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





#' Density chart for numerical feature by categorical target variable
#'
#' @param DataFrame. data.frame. data.frame under investigation.
#' @param NumericalFeature character. Numerical feature
#' @param TargetVariable character. Categorical target variable.
#'
#' @return density plot.
#' @export
#'
#' @examples
#' NumericalFeatureByTargetVariable(DataFrame = iris,
#'                                  NumericalFeature = "Sepal.Length",
#'                                  TargetVariable = "Species")
NumericalFeatureByTargetVariable <- function(DataFrame, NumericalFeature, TargetVariable) {
  # TODO: Overall density (with switch)
  ggplot(data = DataFrame, aes(x = DataFrame[, NumericalFeature], fill = factor(DataFrame[, TargetVariable]))) +
    geom_density(alpha = .2) +
    theme(legend.position = "bottom")
}




#' Count the number of missing values in each variable in a data.frame
#'
#' @description Counts the number of missing values of each variable in a
#' data.frame and returns either absolute or relative values
#' (argument `absoluteValues`)
#' @param DataFrame data.frame. Contains the variables which should be
#' investigated.
#' @param absoluteValues. boolean. If TRUE, absolute values will be printed out,
#' if FALSE, relative values. Default are absolute values.
#'
#' @return data.frame. Returns a data.frame with variable names, number of valid
#' cases and number of missing cases.
#' @export
#'
#' @examples
#' ## dataset airquality contains NAs
#' airquality[1:8,]
#' # absolute values
#' FreqTableMissingValues(DataFrame = airquality, absoluteValues = TRUE, chart = FALSE)
#' # relative values with chart
#' FreqTableMissingValues(DataFrame = airquality, absoluteValues = FALSE, chart = TRUE)
FreqTableMissingValues <- function(DataFrame, absoluteValues = TRUE, chart = FALSE) {
  ## is.na() will be used to get the number of missing values in each variable
  #   result is a data.frame
  vectorMissingValues <- sapply(X = DataFrame, FUN = function(x) {sum(is.na(x))})
  ## Build a data.frame
  if (absoluteValues) {
    MissingValuesVariables <- data.frame(ValidCases = nrow(DataFrame) - vectorMissingValues,
                                         MissingCases = vectorMissingValues)
  } else {
    MissingValuesVariables <- data.frame(ValidCases = (nrow(DataFrame) - vectorMissingValues)/nrow(DataFrame),
                                         MissingCases = vectorMissingValues/nrow(DataFrame))
  }
  ## Chart
  if (chart) {
    #   Please note: the chart requires relative values!
    RelativeValuesChart <- data.frame(ValidCases = (nrow(DataFrame) - vectorMissingValues)/nrow(DataFrame),
                                      MissingCases = vectorMissingValues/nrow(DataFrame))

    # Generate Data
    RelativeValuesChart$VarNames <- rownames(RelativeValuesChart)
    # Lock in factor level order (reversed order of levels necessary here!)
    RelativeValuesChart$VarNames <- factor(RelativeValuesChart$VarNames,
                                           levels = rev(RelativeValuesChart$VarNames))
    # Plot relative number by variable
    print(ggplot(data = RelativeValuesChart, aes(x = VarNames, y = ValidCases)) +
      geom_bar(stat= "identity", fill = "#0066B3", width = 0.8) +
      xlab("") +
      ylab("Relative number of valid cases") +
      coord_flip())
  }

  ## Return
  return(MissingValuesVariables)
}




#' Statistical Parameter for interval level (and maybe ordinal level) variables
#'
#' @param variable numerical. Input variable, numerical one.
#' @param expandedSet boolean. Calculate skewness and kurtosis?
#' Default is FALSE. Pleae note: Package psych is required!
#' @param densityPlot boolean. Print out a simple density plot?
#'
#' @return data.frame. Returns a data.frame with requested statistical
#' parameter.
#' @export
#'
#' @examples
#' ## Example with missing values, exypanded set and density plot
#' StatisticalParameterIntervalLevel(variable = airquality$Ozone,
#'                                   expandedSet = TRUE,
#'                                   densityPlot = TRUE)
StatisticalParameterIntervalLevel <- function(variable,
                                              expandedSet = FALSE,
                                              densityPlot = FALSE) {
  ## Variable must be a numerical variable.
  ## Note on ordinal scaled variables: User has to decide if function should be
  #   applied to ordinal scaled variables. In R user has to transform ordinal
  #   scaled variables ("ordered") in a numerical one first.

  ## Basic set of parameters
  #  Number of valid cases
  #  Percent value of valid cases
  #  Minimum
  #  Maximum
  #  Arithmetic Mean
  #  Standard Deviation
  #  Median
  basicSet <- c(ValidCases = sum(!is.na(variable)),
                pValidCases = sum(!is.na(variable))/length(variable),
                Min = min(variable, na.rm = TRUE),
                Max = max(variable, na.rm = TRUE),
                Average = mean(variable, na.rm = TRUE),
                StandDev = sd(variable, na.rm = TRUE),
                Median = median(variable, na.rm = TRUE))
  ## Expanded Set:?
  #  Skewness
  #  Kurtosis
  if (expandedSet) {
    basicSet <- c(basicSet,
                  Skewness = psych::skew(variable, na.rm = TRUE),
                  Kurtosis = psych::kurtosi(variable, na.rm = TRUE))
  }

  ## Density Plot:?
  if (densityPlot) {
    plot(density(x = variable, na.rm = T),
         col = "darkgrey",
         lwd = 2,
         main = "Density Plot")
  }

  ## Return a data.frame
  return(data.frame(Variable = basicSet))
  }




#' Average of one or more numerical features by a categorical (target) variable
#'
#' @param NumericalVariables character. Vector of names of numerical variables.
#' @param CategoricalVariable character. Name of single categorical (target) variable.
#' @param DataFrame data.frame. Input data.frame. Has to contain the `NumericalVariables`
#' and the single `CategoricalVariable`
#'
#' @return
#' @export
#'
#' @examples
#' # a few numerical features
#' AverageByCategoricalVariable(NumericalVariables = c("Sepal.Length", "Sepal.Width"),
#'                              CategoricalVariable = c("Species"),
#'                              DataFrame = iris)
#' # a single numerical feature
#' AverageByCategoricalVariable(NumericalVariables = c("mpg"),
#'                              CategoricalVariable = c("am"),
#'                              DataFrame = mtcars)
AverageByCategoricalVariable <- function(NumericalVariables,
                                         CategoricalVariable,
                                         DataFrame) {
  # Calculate the table
  TableArithmeticMean <- aggregate(x = DataFrame[,NumericalVariables, drop = FALSE],
                                   by = list(DataFrame[,CategoricalVariable]),
                                   FUN = "mean",
                                   na.rm = TRUE)
  # Set correct group name in data.frame
  colnames(TableArithmeticMean)[1] <- CategoricalVariable
  # return
  return(TableArithmeticMean)
}




