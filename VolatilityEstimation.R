#Ez a függvény napi OLHC-adatok alapján becsült napi volatilitást.
#A képlet Diebold 2009-es cikkébõl lett átvéve (https://onlinelibrary.wiley.com/doi/full/10.1111/j.1468-0297.2008.02208.x?casa_token=93RIrkkUi_wAAAAA%3AjeGSl_LG16MuiGmhGaZ5pm8j-Ih5adgBSoEmPybxxyVsaiwVi751KloILcAAW-_y_xWWCszf3jueta0)

VolatilityEst <- function(X){
  X$Analysis$RawData <- data.frame(matrix(0,ncol=X$ExcellLoad$ExcellParameters$AssetNum, nrow=nrow(X$Analysis$FilteredData)))
  
  for (i in 0:(X$ExcellLoad$ExcellParameters$AssetNum-1)){
    X$Analysis$RawData[,i+1] <-0.511*(X$Analysis$FilteredData[,1+4*i+3]-X$Analysis$FilteredData[,1+4*i+4])^2-0.019*((X$Analysis$FilteredData[,1+4*i+1]-X$Analysis$FilteredData[,1+4*i+2])*(X$Analysis$FilteredData[,1+4*i+3]+X$Analysis$FilteredData[,1+4*i+4]-2*X$Analysis$FilteredData[,1+4*i+2])-2*(X$Analysis$FilteredData[,1+4*i+3]-X$Analysis$FilteredData[,1+4*i+2])*(X$Analysis$FilteredData[,1+4*i+4]-X$Analysis$FilteredData[,1+4*i+2]))-0.383*(X$Analysis$FilteredData[,1+4*i+1]-X$Analysis$FilteredData[,1+4*i+2])^2
  }
  return(X)
}