#Ez a függvény oksági vizsgálatot végez az X$Analysis$RawData adatokon. Az oksági mérõszám típusát a LoadAnalysisParameters függvényben adhatjuk meg.

CausalityAnalysis <- function(X) {
  if (X$Analysis$AnalysisParameters$CasType == "Granger"){
    X$Analysis$Granger <- array(0,dim = c(X$ExcellLoad$ExcellParameters$AssetNum,X$ExcellLoad$ExcellParameters$AssetNum))
    for (i in (1:X$ExcellLoad$ExcellParameters$AssetNum)) {
      if (i+1<X$ExcellLoad$ExcellParameters$AssetNum){
        for (j in ((i+1):X$ExcellLoad$ExcellParameters$AssetNum )) {
          if(lmtest::grangertest(X$Analysis$RawData[,i],X$Analysis$RawData[,j],na.action= na.omit,order = X$Analysis$AnalysisParameters$Lag)[2,4]<X$Analysis$AnalysisParameters$SignLev){
            X$Analysis$Granger[i,j] <- 1
          } else {
            X$Analysis$Granger[i,j] <- 0
          }
          if(lmtest::grangertest(X$Analysis$RawData[,j],X$Analysis$RawData[,i],na.action= na.omit,order = X$Analysis$AnalysisParameters$Lag)[2,4]<X$Analysis$AnalysisParameters$SignLev){
            X$Analysis$Granger[j,i] <- 1
          } else {
            X$Analysis$Granger[j,i] <- 0
          }
        }
      }
    }
    colnames(X$Analysis$Granger) <- X$ExcellLoad$Assetlist
    rownames(X$Analysis$Granger) <- X$ExcellLoad$Assetlist
    return (X)
  }
}