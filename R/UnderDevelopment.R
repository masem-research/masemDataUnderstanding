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
