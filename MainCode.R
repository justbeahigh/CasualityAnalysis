#Ez a script a szükséges package-k telepítése ezután a függvények futtatása után egy oksági kapcsolatot vizsgál a bemeneti eszközök becsült volatilitásán (OLHC adatokból becsült volatilitás)


install.packages("dplyr")
install.packages("lubridate")
install.packages("readxl")
install.packages("tidyr")

X <- list("ExcellLoad","Analysis")
X <- LoadExcellParameters(X)
X <- LoadExcellData(X)
X <- LoadAnalysisParameters(X)
X <- VolatilityEst(X)
X <- CausalityAnalysis(X)
X <- AnalysisSummary(X)