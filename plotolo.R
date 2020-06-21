rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("qgraph")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#add meg melyik mappában van az input fileod
setwd('C:/tdk_excell')


#az inputnak egy táblázatnak kell lennie, nekem elsõ sorban vannak az eszköznevek
f = read.xlsx("overadata.xlsx",sheetName = "overallstatik" )
names.fi = colnames(f)

#ez csak akkor kell ha az átló feletti/alatti részek pont fordítva voltak
#con      = t(as.matrix(read.xlsx("overadata.xlsx",sheetName = "overallstatik" )))

#az inputban mettõl meddig vannak a csoportok
groups   = list(1:5, 6:10, 11:15, 16:20, 21:25, 26:30, 31:35)
#a nyilak és karikák színei, annyit kell megadni ahány csoport van
col      = c(rep("red", 5), rep("blue", 5), rep("green4", 5), rep("darkorange4",5),rep("darkorchid",5),rep("cyan2",5),rep("darkgoldenrod2",5))

#ezt nem kötelezõ, itt csak szûkíted, hogy mit ábrázolsz, megadhatod milyen érték felett ábrázoljon csak
#hasznos a jobb átláthatósághoz 
#con = ifelse(abs(con) >= 0.05,con, 0)

#a karikák kitöltési színei, annyi színt kell megadni ahány Csoport van
col2 <- c("white","white","white","white","white","white","white")
#a karikék szövegeinek a színei, annyi színt kell megadni ahány csoport van
col3 <- c("black", "black", "black", "black", "black", "black", "black")



plot_g = qgraph(con, groups = groups, layout = "groups", layoutScale = c(1, 1), 
                label.font = 2, label.cex = 1.5, shape = "circle", labels = names.fi, esize = 5, 
                maximum = max(con), color = col2, node.width = 0.8, label.cex = 1.8, label.color = col, 
                edge.color = col, curve = 1, border.width = 1.2, border.color = col, asize = 2)



#az ábra címe
text(x = -0.9, y = 1, labels = substitute(paste("2000 - 2020")),
     xpd = NA, cex = 1.5)
#jelmagyarázat
legend(0.82, 1.1, box.lwd = 2, box.col = "white", bg = "white", c("Depositories", 
                                                                  "Insurers", "Broker-Dealers", "Others"), text.col = c("red", "blue", "green4", 
                                                                                                                        "purple3"), cex = 1.3)
