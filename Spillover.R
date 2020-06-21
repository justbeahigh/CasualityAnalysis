X$Analysis$FilteredData
adat1 <- X$Analysis$RawData[1:2615,]
adat2 <- X$Analysis$RawData[2616:5230,]

colnames(X$Analysis$RawData) <- X$ExcellLoad$Assetlist

params_est = list(p = 2, type = "const")
eredmeny$statikus[[10]] <- spilloverDY12(VAR(adat2,p=2),n.ahead = 10,no.corr = F)
eredmeny$dinamikus[[8]] <- spilloverRollingDY12(X$Analysis$RawData,n.ahead = 10,no.corr = F,window = 200,"VAR",params_est = params_est)

eredmeny <-  list()

for (k in c(0:6)){
  eredmeny$dinamikus[[k+1]] <- spilloverRollingDY12(X$Analysis$RawData[,(1+k*5):(5+k*5)],n.ahead = 10,no.corr = F,window = 200,"VAR",params_est = params_est)
}


for (k in c(0:6)){
  eredmeny$statikus[[k+1]] <- spilloverDY12(VAR(X$Analysis$RawData[,(1+k*5):(5+k*5)],p=2),n.ahead = 10,no.corr = F)
}


write.csv(overall(eredmeny),file="overallindex.csv")
write.csv(net(eredmeny),file="netindex.csv")
write.csv(pairwise(eredmeny),file="netpairwaise.csv")
write.csv(datumok,file="datumok.csv")

sectors <- as.data.frame(X$Analysis$FilteredData[,1])
sectors <- sectors[-c(1:199),]

for (i in c(1:8)){
  sectors <- cbind(sectors, overall(eredmeny$dinamikus[[i]])[[1]])
}
