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


#' Berechnung einiger zentraler Evaluationsparameter: Vorhersage Metrischer Variablen
#'
#' @param Labels numeric. Vektor der metrisch skalierten Zielvariablen. NA-Werte sind aktuell nicht erlaubt und 
#' müssen noch manuell entfernt werden
#' @param Predictions numeric. Vektor der auf Basis des Machine Learning Models vorhergesagten 
#' metrisch-skalierten Variablen
#'
#' @return character. Named Vektor mit zentralen Evaluationsparametern: MAE, MSE, RMSE und R2
#' @export
#'
#' @examples
#' BerechnungTestStatistiken(Labels = c(0.00, 1.50, 3.50, 6.30, 1.50, 0.00, 3.00, 4.16),
#'                           Predictions = c(0.1653140, 2.8294117, 3.5218980, 5.0807042, 2.9414713, 
#'                                          -0.1564419, 3.0363548, 2.4665890))
BerechnungTestStatistiken <- function(Labels, Predictions) {
  # Residuen berechnen
  Residuals <- Labels - Predictions
  # MSE berechnen:
  mse <-  mean((Residuals)^2)
  # MAE  berechnen:
  mae <- mean(abs(Residuals))
  # RMSE berechnen:
  rmse <- sqrt(mse)
  # R2 berechnen:
  R2 <- 1-(sum((Residuals)^2)/sum((Labels - mean(Labels))^2))
  # Werte auf der Konsole ausgeben:
  cat(" MAE:", mae, "\n", "MSE:", mse, "\n", "RMSE:", rmse, "\n", "R-squared:", R2, "\n\n")
  # Ausgabevektor erzeugen:
  return(c(MAE = mae, MSE = mse, RMSE = rmse, R2 = R2))
  
}



#' Entfernt standardisierte Residuen eines lm-Objekts außerhalb des angegebenen Schwellenwertes 
#'
#' @description Die Funktion hat als Parameter ein lm-Objekt und einen Schwellenwert als z-Wert
#' Zuerst werden die vorhandenen Residuen des lm-Objekts z-standardisiert grafisch darstellt, dann werden
#' Fälle außerhalb des angegebenen Schwellenwertes identifiziert und eine um diese Fälle bereinigte Grafik
#' angezeigt. Es wird ein Filter mit den außerhalb liegenden Fällen als TRUE zurückgegeben.
#'
#' @param lmObjekt lm. lm-Objekt als Ergebnis der lm()-Funktion. 
#' @param SchwellenwertAusschlussStandResiduen numerisch. Schwellenwert in z-Einheiten. default ist 2.
#'
#' @return logical. Logischer Vektor, TRUE stellt einen Wert außerhalb des Schwellenwertes vor. Kann als Filter zur 
#' Bereinigung des  data.frame genutzt werden.
#' 
#' @export
#'
#' @examples
#' mtcarsLMObjekt <- lm(mpg ~ ., data = mtcars)
#' ResiduenBereinigen(lmObjekt = mtcarsLMObjekt, SchwellenwertAusschlussStandResiduen = 2)
ResiduenBereinigen <- function(lmObjekt, SchwellenwertAusschlussStandResiduen) {
  ## lm-Objekt?
  if (!inherits(x = lmObjekt, what = "lm")) {
    stop("Error: Anscheinend liegt kein lm-Objekt vor.")
  }
  ## Numerische Wert für Schwellenwert?
  if (!is.numeric(x = SchwellenwertAusschlussStandResiduen)) {
    stop("Error: Der Schwellenwert ist als numerischer Wert anzugeben.")
  }
  ## Grafik-Fenster vorbereiten: 2 Spalten, 1 Zeile
  par(mfrow = c(1, 2))
  ## Standardisierte Residuen erzeugen
  zStandResiduen <- scale(resid(lmObjekt))
  ## Plot
  hist(x = zStandResiduen, col = "orange", freq = FALSE, 
       main = "z-Stand. Residuen", xlab = "z-Werte")
  lines(density(zStandResiduen), lty = 2)
  ## Schwellenwerte nach Parameter einzeichnen
  abline(v = c(-1*abs(SchwellenwertAusschlussStandResiduen), abs(SchwellenwertAusschlussStandResiduen)), lty= 2)
  ## Filter erstellen: stand. Residuen > Schwellenwert
  FilterStandResiduenSchwellenwert <- abs(zStandResiduen) > abs(SchwellenwertAusschlussStandResiduen)
  ## Statistik:
  message(paste0("Es liegen ", sum(FilterStandResiduenSchwellenwert), 
                " Fälle von insgesamt ", length(FilterStandResiduenSchwellenwert),
                " außerhalb des angegebenen Schwellenwertes."))
  ## Plot ohne ausgeschlossene Fälle
  hist(x = zStandResiduen[!FilterStandResiduenSchwellenwert], col = "yellow", freq = FALSE, 
       main = "z-Stand. Residuen", xlab = "z-Werte")
  lines(density(zStandResiduen[!FilterStandResiduenSchwellenwert]), lty = 2)
  ## Grafik-Fenster wieder zurücksetzen
  par(mfrow = c(1, 1))
  
  ## Filter zurückgeben
  message("Gebe Filter zurück - Werte außerhalb des Schwellenwertes sind TRUE")
  return(FilterStandResiduenSchwellenwert)
  
}



#' Split data.frame in Trainings- und Testdatensatz und Bereinigung 
#' 
#' @description Splittet einen data.frame nach der Split Ratio (default .7) in einen Trainings- und Testdatensatz und 
#' entfernt Variablen ohne Varianz in beiden Splits sowie Zeilen mit NAs
#'
#' @param DataFrame data.frame. data.frame, der in einen Trainings- und Testdatensatz zerlegt werden soll.
#' @param SplitRatio double. Anteil an Fällen im Trainingsdatensatz - **in Punkten anzugeben!** - default: `.7` (70%)
#' @param NAEntfernen logicial. Sollen Zeilen mit fehlenden Werten entfernt wernden? Es findet ein sog. 
#' Fallweiser (listwise) Ausschluss statt, d.h. der ganze Fall wird entfernt. default = `TRUE`.
#'
#' @return list. Liste mit den beiden data.frames: Trainings- und Test data.frames.
#' @export
#'
#' @examples
#' TrainingUndTestDataFrame <- SplitUndEntfernenVonKonstanten(DataFrame = AnalyseDatenSatz, SplitRatio = .7, NAEntfernen = TRUE)
#' sapply(X = TrainingUndTestDataFrame, dim)
SplitUndEntfernenVonKonstanten <- function(DataFrame, SplitRatio = .7, NAEntfernen = TRUE) {
  ## Zuerst werden Datensätze mit NAs entfernt, wenn TRUE
  if (NAEntfernen) {
    message(paste0("Es liegen ", nrow(DataFrame) - sum(complete.cases(DataFrame)), " Fälle mit fehlenden Werten vor. 
                   \nDiese werden entfernt."))
    DataFrame <- DataFrame[complete.cases(DataFrame),]
  }
  ## Split DataFrame nach Ratio
  IDSampleTraining <- sample(x = 1:nrow(DataFrame), size = round(nrow(DataFrame)*SplitRatio), replace = FALSE)
  ## Aufteilen des Data.Frame
  DataFrameTraining <- DataFrame[IDSampleTraining,]
  DataFrameTest <- DataFrame[-IDSampleTraining,]
  message(paste0("Im Trainingsdatensatz befinden sich ", nrow(DataFrameTraining), " Fälle\n", 
                 "Im Testdatensatz befinden sich ", nrow(DataFrameTest), " Fälle.\n"))  
  ## Prüfen, welche Variablen in beiden Teildatensätzen keine Varianz aufweisen
  message(paste0("Im Trainingsdatensatz weisen ", sum(sapply(X = DataFrameTraining, FUN = sd, na.rm = T) == 0), 
                 " Variablen keine Varianz auf.\n",
                 "Im Testdatensatz weisen ", sum(sapply(X = DataFrameTest, FUN = sd, na.rm = T) == 0), 
                 " Variablen keine Varianz auf.\n"))
  ## Kombinierten Vektor mit Variablennamen erstellen, die keine Varianz aufweisen
  VariablenOhneVarianz <- unique(c(colnames(DataFrameTraining)[sapply(X = DataFrameTraining, FUN = sd, na.rm = T) == 0],
                                   colnames(DataFrameTest)[sapply(X = DataFrameTest, FUN = sd, na.rm = T) == 0]))
  ## Datensatz bereinigen
  DataFrameTraining <- DataFrameTraining[,!colnames(DataFrameTraining) %in% VariablenOhneVarianz]
  DataFrameTest <- DataFrameTest[,!colnames(DataFrameTest) %in% VariablenOhneVarianz]
  ## Datensätze als Liste zurückgeben
  return(list(DataFrameTraining = DataFrameTraining, DataFrameTest = DataFrameTest))
}
