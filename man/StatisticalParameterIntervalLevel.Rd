% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataUnderstanding.R
\name{StatisticalParameterIntervalLevel}
\alias{StatisticalParameterIntervalLevel}
\title{Statistical Parameter for interval level (and maybe ordinal level) variables}
\usage{
StatisticalParameterIntervalLevel(
  variable,
  expandedSet = FALSE,
  densityPlot = FALSE
)
}
\arguments{
\item{variable}{numerical. Input variable, numerical one.}

\item{expandedSet}{boolean. Calculate skewness and kurtosis?
Default is FALSE. Pleae note: Package psych is required!}

\item{densityPlot}{boolean. Print out a simple density plot?}
}
\value{
data.frame. Returns a data.frame with requested statistical
parameter.
}
\description{
Statistical Parameter for interval level (and maybe ordinal level) variables
}
\examples{
## Example with missing values, exypanded set and density plot
StatisticalParameterIntervalLevel(variable = airquality$Ozone,
                                  expandedSet = TRUE,
                                  densityPlot = TRUE)
}
