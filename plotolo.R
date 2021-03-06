rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("qgraph")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#add meg melyik mapp�ban van az input fileod
setwd('C:/tdk_excell')


#az inputnak egy t�bl�zatnak kell lennie, nekem els� sorban vannak az eszk�znevek
f = read.xlsx("overadata.xlsx",sheetName = "overallstatik" )
names.fi = colnames(f)

#ez csak akkor kell ha az �tl� feletti/alatti r�szek pont ford�tva voltak
#con      = t(as.matrix(read.xlsx("overadata.xlsx",sheetName = "overallstatik" )))

#az inputban mett�l meddig vannak a csoportok
groups   = list(1:5, 6:10, 11:15, 16:20, 21:25, 26:30, 31:35)
#a nyilak �s karik�k sz�nei, annyit kell megadni ah�ny csoport van
col      = c(rep("red", 5), rep("blue", 5), rep("green4", 5), rep("darkorange4",5),rep("darkorchid",5),rep("cyan2",5),rep("darkgoldenrod2",5))

#ezt nem k�telez�, itt csak sz�k�ted, hogy mit �br�zolsz, megadhatod milyen �rt�k felett �br�zoljon csak
#hasznos a jobb �tl�that�s�ghoz 
#con = ifelse(abs(con) >= 0.05,con, 0)

#a karik�k kit�lt�si sz�nei, annyi sz�nt kell megadni ah�ny Csoport van
col2 <- c("white","white","white","white","white","white","white")
#a karik�k sz�vegeinek a sz�nei, annyi sz�nt kell megadni ah�ny csoport van
col3 <- c("black", "black", "black", "black", "black", "black", "black")



plot_g = qgraph(con, groups = groups, layout = "groups", layoutScale = c(1, 1), 
                label.font = 2, label.cex = 1.5, shape = "circle", labels = names.fi, esize = 5, 
                maximum = max(con), color = col2, node.width = 0.8, label.cex = 1.8, label.color = col, 
                edge.color = col, curve = 1, border.width = 1.2, border.color = col, asize = 2)



#az �bra c�me
text(x = -0.9, y = 1, labels = substitute(paste("2000 - 2020")),
     xpd = NA, cex = 1.5)
#jelmagyar�zat
legend(0.82, 1.1, box.lwd = 2, box.col = "white", bg = "white", c("Depositories", 
                                                                  "Insurers", "Broker-Dealers", "Others"), text.col = c("red", "blue", "green4", 
                                                                                                                        "purple3"), cex = 1.3)
