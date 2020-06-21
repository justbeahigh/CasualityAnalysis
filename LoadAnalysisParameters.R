#A függvény bekéri az oksági vizsgálathoz szükséges paramétereket, majd az X$Analysis$FilteredData listaelembe leszûri a dátumok alapján az adatokat.

LoadAnalysisParameters <- function(X){
  X$Analysis$AnalysisParameters$StartDate <- "2000.01.01"
  X$Analysis$AnalysisParameters$EndDate <- "2020.01.17"
  X$Analysis$AnalysisParameters$Lag <- 2
  X$Analysis$AnalysisParameters$SignLev <- 0.05
  X$Analysis$AnalysisParameters$CasType <- "Granger"
  X$Analysis$AnalysisParameters$WindowSize <- 200
  X$Analysis$AnalysisParameters$WindowLag <- 20
  
  X$Analysis$FilteredData <- as.data.frame(na.omit(X$ExcellLoad$BaseData[X$ExcellLoad$BaseData$Dates >= lubridate::as_date(X$Analysis$AnalysisParameters$StartDate) & X$ExcellLoad$BaseData$Dates <= lubridate::as_date(X$Analysis$AnalysisParameters$EndDate),]))
  X$Analysis$FilteredData[,2:(ncol(X$Analysis$FilteredData))] <- log(X$Analysis$FilteredData[,2:(ncol(X$Analysis$FilteredData))])
  return(X)
}