#Ez a f�ggv�ny egy Granger oks�gi m�trixot �sszegez t�bl�zatokba. A k�vetkez� m�don kell kin�zzen a bemeneti adat:
#-a sorok �s oszlopok a vizsg�lt eszk�z�k
#-a t�bl�zatban 0-1 adatok szerepelnek. 1 akkor, ha a sor Granger-okozza az oszlopot. Ha ez nem teljes�l akkor 0
#- a f��tl�ban 0 szerepelnek
#-35 eszk�zt vizsg�l 7 szektorra bontva �s az egy szektorban l�v� szerepl�k egym�s ut�n k�vetkeznek a sorokban �s oszlopokban is


AnalysisSummary <- function (X){
  X$Summary1 <- data.frame(matrix(0, nrow= X$ExcellLoad$ExcellParameters$AssetNum, ncol=4), row.names = X$ExcellLoad$Assetlist)
  for (i in 1:35){
    X$Summary[i,1] <- sum(X$Analysis$Granger[i,])
    X$Summary[i,2] <- sum(X$Analysis$Granger[,i])
    X$Summary[i,3] <- sum(X$Analysis$Granger[,i]) + sum(X$Analysis$Granger[i,])
    if (i <6){
      X$Summary[i,4] <- "P�nz�gy"  
    } else if (i<11) {
      X$Summary[i,4] <- "Informatika" 
    } else if (i<16) {
      X$Summary[i,4] <- "Energia" 
    } else if (i<21) {
      X$Summary[i,4] <- "Nem alapvet� fogyaszt�si cikkek" 
    } else if (i<26) {
      X$Summary[i,4] <- "Alapvet� fogyaszt�si cikkek" 
    } else if (i<31) {
      X$Summary[i,4] <- "Eg�szs�g�gy" 
    } else {
      X$Summary[i,4] <- "T�vk�zl�si szektor"
    }
  }
  X$Summary2 <- data.frame(matrix(0, nrow= X$ExcellLoad$ExcellParameters$AssetNum, ncol=4), row.names = X$ExcellLoad$Assetlist)
  for (i in 1:35){
    if (i <6){
      X$Summary2[i,1] <- sum(X$Analysis$Granger[1:5,i]) + sum(X$Analysis$Granger[i,1:5])
      X$Summary2[i,2] <- (sum(X$Analysis$Granger[1:5,i]) + sum(X$Analysis$Granger[i,1:5]))/(2*4)
      X$Summary2[i,3] <- sum(X$Analysis$Granger[,i]) + sum(X$Analysis$Granger[i,])-sum(X$Analysis$Granger[1:5,i]) - sum(X$Analysis$Granger[i,1:5])
      X$Summary2[i,4] <- X$Summary2[i,3]/(2*30)
      
    } else if (i<11) {
      X$Summary2[i,1] <- sum(X$Analysis$Granger[6:10,i]) + sum(X$Analysis$Granger[i,6:10])
      X$Summary2[i,2] <- (sum(X$Analysis$Granger[6:10,i]) + sum(X$Analysis$Granger[i,6:10]))/(2*4)
      X$Summary2[i,3] <- sum(X$Analysis$Granger[,i]) + sum(X$Analysis$Granger[i,])-sum(X$Analysis$Granger[6:10,i]) - sum(X$Analysis$Granger[i,6:10])
      X$Summary2[i,4] <- X$Summary2[i,3]/(2*30) 
    } else if (i<16) {
      X$Summary2[i,1] <- sum(X$Analysis$Granger[11:15,i]) + sum(X$Analysis$Granger[i,11:15])
      X$Summary2[i,2] <- (sum(X$Analysis$Granger[11:15,i]) + sum(X$Analysis$Granger[i,11:15]))/(2*4)
      X$Summary2[i,3] <- sum(X$Analysis$Granger[,i]) + sum(X$Analysis$Granger[i,])-sum(X$Analysis$Granger[11:15,i]) - sum(X$Analysis$Granger[i,11:15])
      X$Summary2[i,4] <- X$Summary2[i,3]/(2*30) 
    } else if (i<21) {
      X$Summary2[i,1] <- sum(X$Analysis$Granger[16:20,i]) + sum(X$Analysis$Granger[i,16:20])
      X$Summary2[i,2] <- (sum(X$Analysis$Granger[16:20,i]) + sum(X$Analysis$Granger[i,16:20]))/(2*4)
      X$Summary2[i,3] <- sum(X$Analysis$Granger[,i]) + sum(X$Analysis$Granger[i,])-sum(X$Analysis$Granger[16:20,i]) - sum(X$Analysis$Granger[i,16:20])
      X$Summary2[i,4] <- X$Summary2[i,3]/(2*30) 
    } else if (i<26) {
      X$Summary2[i,1] <- sum(X$Analysis$Granger[21:25,i]) + sum(X$Analysis$Granger[i,21:25])
      X$Summary2[i,2] <- (sum(X$Analysis$Granger[21:25,i]) + sum(X$Analysis$Granger[i,21:25]))/(2*4)
      X$Summary2[i,3] <- sum(X$Analysis$Granger[,i]) + sum(X$Analysis$Granger[i,])-sum(X$Analysis$Granger[21:25,i]) - sum(X$Analysis$Granger[i,21:25])
      X$Summary2[i,4] <- X$Summary2[i,3]/(2*30) 
    } else if (i<31) {
      X$Summary2[i,1] <- sum(X$Analysis$Granger[26:30,i]) + sum(X$Analysis$Granger[i,26:30])
      X$Summary2[i,2] <- (sum(X$Analysis$Granger[26:30,i]) + sum(X$Analysis$Granger[i,26:30]))/(2*4)
      X$Summary2[i,3] <- sum(X$Analysis$Granger[,i]) + sum(X$Analysis$Granger[i,])-sum(X$Analysis$Granger[26:30,i]) - sum(X$Analysis$Granger[i,26:30])
      X$Summary2[i,4] <- X$Summary2[i,3]/(2*30) 
    } else {
      X$Summary2[i,1] <- sum(X$Analysis$Granger[31:35,i]) + sum(X$Analysis$Granger[i,31:35])
      X$Summary2[i,2] <- (sum(X$Analysis$Granger[31:35,i]) + sum(X$Analysis$Granger[i,31:35]))/(2*4)
      X$Summary2[i,3] <- sum(X$Analysis$Granger[,i]) + sum(X$Analysis$Granger[i,])-sum(X$Analysis$Granger[31:35,i]) - sum(X$Analysis$Granger[i,31:35])
      X$Summary2[i,4] <- X$Summary2[i,3]/(2*30)
    }
  }
  return(X)
}