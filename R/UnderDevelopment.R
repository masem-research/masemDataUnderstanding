## Function to load a package or - if not available - install the package. 
#   If package is not installable, will throw an error
load.package <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Load the fine mlr3 framework
load.package("mlr3")




## Function to round like we learned it in school :-)
#   0.5 will be rounded up to 1
#   (round() function in R will round 0.5 to 0)
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}
