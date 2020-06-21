#A függvényben megadhatjuk a használandó excellfile megadott paramétereit.

LoadExcellParameters <- function(X){
  X$ExcellLoad$ExcellParameters$Header <- TRUE
  X$ExcellLoad$ExcellParameters$FirstToRead <- 0
  X$ExcellLoad$ExcellParameters$DateType <- "%y.%m.%d"
  X$ExcellLoad$ExcellParameters$Path <- "C:\\Users\\Gergõ\\Desktop\\efrp\\beszamolo\\RawDataOnly3.xlsx"
  X$ExcellLoad$ExcellParameters$AssetNum <- 35
  
  return(X)
}