#Ez a script a sz�ks�ges package-k telep�t�se ezut�n a f�ggv�nyek futtat�sa ut�n egy oks�gi kapcsolatot vizsg�l a bemeneti eszk�z�k becs�lt volatilit�s�n (OLHC adatokb�l becs�lt volatilit�s)


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