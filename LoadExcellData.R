#A LoadExcellParameters függvény alapján betölti az R-be az excelladatokat.
LoadExcellData <- function(X){
  X$ExcellLoad$BaseData <- na.omit(readxl::read_xlsx(X$ExcellLoad$ExcellParameters$Path, col_names = X$ExcellLoad$ExcellParameters$Header))
  X$ExcellLoad$Assetlist <- c()
  for (i in 0:(X$ExcellLoad$ExcellParameters$AssetNum-1)) {
    X$ExcellLoad$Assetlist <- c(X$ExcellLoad$Assetlist,colnames(X$ExcellLoad$BaseData)[4*i+2])
  }
  return(X)
}